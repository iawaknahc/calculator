open Types

type item =
  | S_Buffer of Buffer.t
  | S_Op of Op.t

(*
 * The length of the list must be 0, 1, 2, 3, 4 or 5.
 * When the length is 6, we can always simplify to make it become 4.
 * When the length is 5, we may simplify to make it become 3.
 *)
type t = item list

let empty = []

let simplify = function
  | [ S_Buffer c; S_Op op2; S_Buffer b; S_Op op1; S_Buffer a ] as stack ->
      if Op.(op1 >= op2)
      then
        let a = Buffer.to_float a in
        let b = Buffer.to_float b in
        let d = Op.apply op1 a b in
        [ S_Buffer c; S_Op op2; S_Buffer (Buffer.new_result d) ]
      else stack
  | [ S_Op op3; S_Buffer c; S_Op op2; S_Buffer b; S_Op op1; S_Buffer a ] ->
      let b = Buffer.to_float b in
      let c = Buffer.to_float c in
      let d = Op.apply op2 b c in
      [ S_Op op3; S_Buffer (Buffer.new_result d); S_Op op1; S_Buffer a ]
  | stack ->
      stack


let append_number stack ~max ~n =
  match stack with
  | S_Buffer b :: rest ->
      S_Buffer (Buffer.append_number b max n) :: rest
  | rest ->
      S_Buffer (Buffer.new_number n) :: rest |> simplify


let append_dot = function
  | S_Buffer b :: rest ->
      S_Buffer (Buffer.append_dot b) :: rest
  | rest ->
      S_Buffer Buffer.new_dot :: rest |> simplify


let append_op stack op =
  match stack with
  | [] ->
      [ S_Op op; S_Buffer (Buffer.new_number 0) ]
  | S_Op _ :: rest ->
      S_Op op :: rest
  | rest ->
      S_Op op :: rest |> simplify


let negate = function
  | S_Buffer b :: rest ->
      S_Buffer (Buffer.toggle_sign b) :: rest
  | rest ->
      S_Buffer Buffer.new_sign :: rest |> simplify


let cancel = function
  | S_Op _ :: _ as rest ->
      S_Buffer (Buffer.new_number 0) :: rest |> simplify
  | S_Buffer _ :: rest ->
      S_Buffer (Buffer.new_number 0) :: rest
  | rest ->
      rest


let top_display = function
  | [] ->
      "0"
  | S_Buffer b :: _ ->
      Buffer.format b
  | [ S_Op op2; S_Buffer b; S_Op op1; S_Buffer a ] ->
      if Op.(op1 >= op2)
      then
        let a = Buffer.to_float a in
        let b = Buffer.to_float b in
        Misc.format_float (Op.apply op1 a b)
      else Buffer.format b
  | [ S_Op _; S_Buffer a ] ->
      Buffer.format a
  | _ ->
      failwith "unreachable"


let eval = function
  | [] ->
      0.0
  | [ S_Buffer b ] ->
      Buffer.to_float b
  | [ S_Buffer c; S_Op op2; S_Buffer b; S_Op op1; S_Buffer a ] ->
      let a = Buffer.to_float a in
      let b = Buffer.to_float b in
      let c = Buffer.to_float c in
      if Op.(op1 >= op2)
      then Op.apply op2 (Op.apply op1 a b) c
      else Op.apply op1 a (Op.apply op2 b c)
  | [ S_Buffer b; S_Op op; S_Buffer a ] ->
      let a = Buffer.to_float a in
      let b = Buffer.to_float b in
      Op.apply op a b
  | _ ->
      failwith "unreachable"


let last_operation = function
  | S_Buffer b :: S_Op op :: _ ->
      Some (op, Buffer.to_float b)
  | _ ->
      None


let eq = function
  | S_Buffer _ :: _ as stack ->
      let last_operation = last_operation stack in
      let result = eval stack in
      let buf = Buffer.new_result result in
      ([ S_Buffer buf ], last_operation)
  | S_Op _ :: _ as stack ->
    ( try
        let operand =
          top_display stack |> float_of_string |> Buffer.new_result
        in
        let stack = S_Buffer operand :: stack in
        let last_operation = last_operation stack in
        let result = eval stack in
        let buf = Buffer.new_result result in
        ([ S_Buffer buf ], last_operation)
        (* float_of_string may fail due to "Error" *)
      with
    | Failure _ ->
        ([ S_Buffer (Buffer.new_result nan) ], None) )
  | [] ->
      ([], None)


let repeat stack (operator, b) =
  match stack with
  | S_Buffer buf :: rest ->
      let a = Buffer.to_float buf in
      let result = Op.apply operator a b in
      let buf = Buffer.new_result result in
      S_Buffer buf :: rest
  | _ ->
      stack


let percent = function
  | [] ->
      []
  (* a % => a/100 *)
  | [ S_Buffer a ] ->
      let a = Buffer.to_float a in
      let result = Op.apply Op_Div a 100.0 in
      let buf = Buffer.new_result result in
      [ S_Buffer buf ]
  (* a + % => a + ((a * a) / 100) *)
  (* a - % => a - ((a * a) / 100) *)
  | [ S_Op (Op_Add as op); S_Buffer a ] | [ S_Op (Op_Sub as op); S_Buffer a ]
    ->
      let af = Buffer.to_float a in
      let b = Op.apply Op_Div (Op.apply Op_Mul af af) 100.0 in
      [ S_Buffer (Buffer.new_result b); S_Op op; S_Buffer a ]
  (* a * % => a * (a / 100) *)
  (* a / % => a / (a / 100) *)
  | [ S_Op (Op_Mul as op); S_Buffer a ] | [ S_Op (Op_Div as op); S_Buffer a ]
    ->
      let af = Buffer.to_float a in
      let b = Op.apply Op_Div af 100.0 in
      [ S_Buffer (Buffer.new_result b); S_Op op; S_Buffer a ]
  (* a + b % => a + (a * b / 100) *)
  (* a - b % => a - (a * b / 100) *)
  | [ S_Buffer b; S_Op (Op_Add as op); S_Buffer a ]
  | [ S_Buffer b; S_Op (Op_Sub as op); S_Buffer a ] ->
      let bf = Buffer.to_float b in
      let af = Buffer.to_float a in
      let c = Op.apply Op_Div (Op.apply Op_Mul af bf) 100.0 in
      [ S_Buffer (Buffer.new_result c); S_Op op; S_Buffer a ]
  (* a * b % => a * (b / 100) *)
  (* a / b % => a / (b / 100) *)
  | [ S_Buffer b; S_Op (Op_Mul as op); S_Buffer a ]
  | [ S_Buffer b; S_Op (Op_Div as op); S_Buffer a ] ->
      let bf = Buffer.to_float b in
      let c = Op.apply Op_Div bf 100.0 in
      [ S_Buffer (Buffer.new_result c); S_Op op; S_Buffer a ]
  (* a * b + % => a * b + (a * (b * b) / 100) *)
  (* a * b - % => a * b - (a * (b * b) / 100) *)
  | [ S_Op (Op_Add as op); S_Buffer b; S_Op Op_Mul; S_Buffer a ]
  | [ S_Op (Op_Sub as op); S_Buffer b; S_Op Op_Mul; S_Buffer a ] ->
      let bf = Buffer.to_float b in
      let af = Buffer.to_float a in
      let c =
        Op.apply Op_Mul af (Op.apply Op_Div (Op.apply Op_Mul bf bf) 100.0)
      in
      [ S_Buffer (Buffer.new_result c)
      ; S_Op op
      ; S_Buffer b
      ; S_Op Op_Mul
      ; S_Buffer a
      ]
  (* a / b + % => a / b + (a / 100) *)
  (* a / b - % => a / b - (a / 100) *)
  | [ S_Op (Op_Add as op); S_Buffer b; S_Op Op_Div; S_Buffer a ]
  | [ S_Op (Op_Sub as op); S_Buffer b; S_Op Op_Div; S_Buffer a ] ->
      let af = Buffer.to_float a in
      let c = Op.apply Op_Div af 100.0 in
      [ S_Buffer (Buffer.new_result c)
      ; S_Op op
      ; S_Buffer b
      ; S_Op Op_Div
      ; S_Buffer a
      ]
  (* a + b - % => a + b - (((a + b) * b) / 100) *)
  (* a - b + % => a - b + (((a - b) * b) / 100) *)
  | [ S_Op (Op_Add as op2); S_Buffer b; S_Op op1; S_Buffer a ]
  | [ S_Op (Op_Sub as op2); S_Buffer b; S_Op op1; S_Buffer a ] ->
      let bf = Buffer.to_float b in
      let af = Buffer.to_float a in
      let c =
        Op.apply Op_Div (Op.apply Op_Mul (Op.apply op1 af bf) bf) 100.0
      in
      [ S_Buffer (Buffer.new_result c)
      ; S_Op op2
      ; S_Buffer b
      ; S_Op op1
      ; S_Buffer a
      ]
  (* a op b + c % => a op b + ((a op b) * c / 100) *)
  (* a op b - c % => a op b - ((a op b) * c / 100) *)
  | [ S_Buffer c; S_Op (Op_Add as op2); S_Buffer b; S_Op op1; S_Buffer a ]
  | [ S_Buffer c; S_Op (Op_Sub as op2); S_Buffer b; S_Op op1; S_Buffer a ] ->
      let cf = Buffer.to_float c in
      let bf = Buffer.to_float b in
      let af = Buffer.to_float a in
      let d =
        Op.apply Op_Div (Op.apply Op_Mul (Op.apply op1 af bf) cf) 100.0
      in
      [ S_Buffer (Buffer.new_result d)
      ; S_Op op2
      ; S_Buffer b
      ; S_Op op1
      ; S_Buffer a
      ]
  (* a op b * c % => a op b + (c / 100) *)
  (* a op b / c % => a op b - (c / 100) *)
  | [ S_Buffer c; S_Op (Op_Mul as op2); S_Buffer b; S_Op op1; S_Buffer a ]
  | [ S_Buffer c; S_Op (Op_Div as op2); S_Buffer b; S_Op op1; S_Buffer a ] ->
      let cf = Buffer.to_float c in
      let d = Op.apply Op_Div cf 100.0 in
      [ S_Buffer (Buffer.new_result d)
      ; S_Op op2
      ; S_Buffer b
      ; S_Op op1
      ; S_Buffer a
      ]
  | _ ->
      failwith "unreachable"
