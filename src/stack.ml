type item =
  [ `Buffer of Buffer.t
  | `Op of Op.t ]

(*
 * The length of the list must be 0, 1, 2, 3, 4 or 5.
 * When the length is 6, we can always simplify to make it become 4.
 * When the length is 5, we may simplify to make it become 3.
 *)
type t = item list

let empty = []

let simplify = function
  | [`Buffer c; `Op op2; `Buffer b; `Op op1; `Buffer a] as stack ->
      if Op.(op1 >= op2)
      then
        let a = Buffer.to_float a in
        let b = Buffer.to_float b in
        let d = Op.apply op1 a b in
        [`Buffer c; `Op op2; `Buffer (Buffer.new_result d)]
      else stack
  | [`Op op3; `Buffer c; `Op op2; `Buffer b; `Op op1; `Buffer a] ->
      let b = Buffer.to_float b in
      let c = Buffer.to_float c in
      let d = Op.apply op2 b c in
      [`Op op3; `Buffer (Buffer.new_result d); `Op op1; `Buffer a]
  | stack ->
      stack


let append_number stack ~max ~n =
  match stack with
  | `Buffer b :: rest ->
      `Buffer (Buffer.append_number b max n) :: rest
  | rest ->
      `Buffer (Buffer.new_number n) :: rest |> simplify


let append_dot = function
  | `Buffer b :: rest ->
      `Buffer (Buffer.append_dot b) :: rest
  | rest ->
      `Buffer Buffer.new_dot :: rest |> simplify


let append_op stack op =
  match stack with
  | [] ->
      [`Op op; `Buffer (Buffer.new_number 0)]
  | `Op _ :: rest ->
      `Op op :: rest
  | rest ->
      `Op op :: rest |> simplify


let negate = function
  | `Buffer b :: rest ->
      `Buffer (Buffer.toggle_sign b) :: rest
  | rest ->
      `Buffer Buffer.new_sign :: rest |> simplify


let cancel = function
  | `Op _ :: _ as rest ->
      `Buffer (Buffer.new_number 0) :: rest |> simplify
  | `Buffer _ :: rest ->
      `Buffer (Buffer.new_number 0) :: rest
  | rest ->
      rest


let top_display = function
  | [] ->
      "0"
  | `Buffer b :: _ ->
      Buffer.format b
  | [`Op op2; `Buffer b; `Op op1; `Buffer a] ->
      if Op.(op1 >= op2)
      then
        let a = Buffer.to_float a in
        let b = Buffer.to_float b in
        Misc.format_float (Op.apply op1 a b)
      else Buffer.format b
  | [`Op _; `Buffer a] ->
      Buffer.format a
  | _ ->
      failwith "unreachable"


let eval = function
  | [] ->
      0.0
  | [`Buffer b] ->
      Buffer.to_float b
  | [`Buffer c; `Op op2; `Buffer b; `Op op1; `Buffer a] ->
      let a = Buffer.to_float a in
      let b = Buffer.to_float b in
      let c = Buffer.to_float c in
      if Op.(op1 >= op2)
      then Op.apply op2 (Op.apply op1 a b) c
      else Op.apply op1 a (Op.apply op2 b c)
  | [`Buffer b; `Op op; `Buffer a] ->
      let a = Buffer.to_float a in
      let b = Buffer.to_float b in
      Op.apply op a b
  | _ ->
      failwith "unreachable"


let last_operation = function
  | `Buffer b :: `Op op :: _ ->
      Some (op, Buffer.to_float b)
  | _ ->
      None


let eq = function
  | `Buffer _ :: _ as stack ->
      let last_operation = last_operation stack in
      let result = eval stack in
      let buf = Buffer.new_result result in
      ([`Buffer buf], last_operation)
  | `Op _ :: _ as stack ->
    ( try
        let operand =
          top_display stack |> float_of_string |> Buffer.new_result
        in
        let stack = `Buffer operand :: stack in
        let last_operation = last_operation stack in
        let result = eval stack in
        let buf = Buffer.new_result result in
        ([`Buffer buf], last_operation)
        (* float_of_string may fail due to "Error" *)
      with Failure _ -> ([`Buffer (Buffer.new_result nan)], None) )
  | [] ->
      ([], None)


let repeat stack (operator, b) =
  match stack with
  | `Buffer buf :: rest ->
      let a = Buffer.to_float buf in
      let result = Op.apply operator a b in
      let buf = Buffer.new_result result in
      `Buffer buf :: rest
  | _ ->
      stack
