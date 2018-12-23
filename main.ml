type button =
  [ `Cancel
  | `Negate
  | `Percent
  | `Div
  | `Mul
  | `Sub
  | `Add
  | `Eq
  | `Dot
  | `Num of int ]

let format_float f =
  match classify_float f with
  | FP_normal | FP_subnormal | FP_zero ->
      Printf.sprintf "%g" f
  | FP_infinite | FP_nan ->
      "Error"


module List = struct
  include List

  let filter_map f =
    let rec aux accu = function
      | [] ->
          rev accu
      | x :: l ->
        (match f x with None -> aux accu l | Some v -> aux (v :: accu) l)
    in
    aux []
end

module Buffer = struct
  type t =
    { sign : float
    ; has_dot : bool
    ; appendable : bool
    ; value : string }

  let to_float buf = buf.sign *. float_of_string buf.value

  let format buf = buf |> to_float |> format_float

  let new_number n =
    {sign = 1.0; has_dot = false; appendable = true; value = string_of_int n}


  let new_sign = {sign = -1.0; has_dot = false; appendable = true; value = "0"}

  let new_dot = {sign = 1.0; has_dot = true; appendable = true; value = "0."}

  let new_result n =
    let sign = if n < 0.0 then -1.0 else 1.0 in
    let has_dot = floor n <> n in
    {sign; has_dot; appendable = false; value = string_of_float (abs_float n)}


  let zero = {sign = 1.0; has_dot = false; appendable = true; value = "0"}

  let toggle_sign buf = {buf with sign = -1.0 *. buf.sign}

  let append_dot buf =
    if not buf.appendable
    then new_dot
    else if buf.has_dot
    then buf
    else {buf with has_dot = true; value = buf.value ^ "."}


  let append_number buf max n =
    if n < 0 || n > 9
    then buf
    else if not buf.appendable
    then new_number n
    else
      match buf.value with
      | "0" ->
          {buf with value = string_of_int n}
      | value ->
          let len = String.length value in
          let len = if buf.has_dot then len - 1 else len in
          if len >= max then buf else {buf with value = value ^ string_of_int n}
end

module Op = struct
  type t =
    [ `Div
    | `Mul
    | `Sub
    | `Add ]

  let max_precedence = 10

  let precedence = function `Div | `Mul -> 10 | `Sub | `Add -> 9

  let apply op a b =
    match op with
    | `Div ->
        if b = 0.0 then nan else a /. b
    | `Mul ->
        a *. b
    | `Sub ->
        a -. b
    | `Add ->
        a +. b
end

module Stack = struct
  type item =
    [ `Buffer of Buffer.t
    | `Op of Op.t ]

  type t = item list

  let rec top_display = function
    | [] ->
        "0"
    | `Buffer b :: _ ->
        Buffer.format b
    | _ :: rest ->
        top_display rest


  let rec eval = function
    | [] ->
        0.0
    | [`Buffer b] ->
        Buffer.to_float b
    | `Op _ :: rest ->
        eval rest
    | `Buffer b :: `Op op :: rest ->
        let b = Buffer.to_float b in
        let f = Op.apply op in
        f (eval rest) b
    | _ ->
        failwith "unreachable"


  let preview stack =
    let ops =
      List.filter_map (function `Op op -> Some op | _ -> None) stack
    in
    let possible =
      match ops with
      | [] ->
          false
      | op :: _ ->
          let p = Op.precedence op in
          List.for_all (fun op -> p = Op.precedence op) ops
    in
    if not possible then None else Some (eval stack)


  let simplify stack precedence =
    match stack with
    | `Buffer b :: `Op op :: `Buffer a :: rest ->
        let pred = Op.precedence op in
        if pred >= precedence && precedence = Op.max_precedence
        then
          let c = Op.apply op (Buffer.to_float a) (Buffer.to_float b) in
          let buf = Buffer.new_result c in
          `Buffer buf :: rest
        else stack
    | _ ->
        stack


  let add_missing_operand = function
    | `Op op :: `Buffer b :: rest ->
        `Buffer b :: `Op op :: `Buffer b :: rest
    | stack ->
        stack


  let last_operation = function
    | `Buffer b :: `Op op :: _ ->
        Some (op, Buffer.to_float b)
    | _ ->
        None
end

type calculator =
  { max_input_number_of_digits : int
  ; ac : bool
  ; display : string
  ; last_operation : (Op.t * float) option
  ; stack : Stack.t }

let make ?(max_input_number_of_digits = 9) () =
  { max_input_number_of_digits
  ; ac = true
  ; display = "0"
  ; last_operation = None
  ; stack = [] }


let clone cal =
  let max_input_number_of_digits = cal.max_input_number_of_digits in
  make ~max_input_number_of_digits ()


let append_number cal n =
  match cal.stack with
  | `Buffer b :: rest ->
      let max = cal.max_input_number_of_digits in
      let b = Buffer.append_number b max n in
      let stack = `Buffer b :: rest in
      let display = Stack.top_display stack in
      {cal with display; stack}
  | rest ->
      let stack = `Buffer (Buffer.new_number n) :: rest in
      let display = Stack.top_display stack in
      {cal with display; stack}


let append_dot cal =
  match cal.stack with
  | `Buffer b :: rest ->
      let stack = `Buffer (Buffer.append_dot b) :: rest in
      let display = Stack.top_display stack in
      {cal with display; stack}
  | rest ->
      let stack = `Buffer Buffer.new_dot :: rest in
      let display = Stack.top_display stack in
      {cal with display; stack}


let append_op cal op =
  match cal.stack with
  | `Op _ :: rest ->
      let stack = `Op op :: rest in
      let display =
        match Stack.preview stack with
        | None ->
            Stack.top_display stack
        | Some f ->
            format_float f
      in
      {cal with display; stack}
  | rest ->
      let stack = Stack.simplify rest (Op.precedence op) in
      let stack = `Op op :: stack in
      let display =
        match Stack.preview stack with
        | None ->
            Stack.top_display stack
        | Some f ->
            format_float f
      in
      {cal with display; stack}


let negate cal =
  match cal.stack with
  | `Buffer b :: rest ->
      let stack = `Buffer (Buffer.toggle_sign b) :: rest in
      let display = Stack.top_display stack in
      {cal with display; stack}
  | rest ->
      let stack = `Buffer Buffer.new_sign :: rest in
      let display = Stack.top_display stack in
      {cal with display; stack}


let cancel cal =
  if cal.ac
  then clone cal
  else
    match cal.stack with
    | `Op _ :: _ as stack ->
        let stack = `Buffer (Buffer.new_number 0) :: stack in
        let display = Stack.top_display stack in
        {cal with display; stack}
    | `Buffer _ :: rest ->
        let stack = `Buffer (Buffer.new_number 0) :: rest in
        let display = Stack.top_display stack in
        {cal with display; stack}
    | _ ->
        cal


let eq cal =
  match cal.last_operation with
  | None ->
      let stack = cal.stack in
      let stack = Stack.add_missing_operand stack in
      let last_operation = Stack.last_operation stack in
      let stack = Stack.simplify stack Op.max_precedence in
      let result = Stack.eval stack in
      let buf = Buffer.new_result result in
      let stack = [`Buffer buf] in
      let display = Stack.top_display stack in
      {cal with display; stack; last_operation}
  | Some (operator, b) ->
    ( match cal.stack with
    | `Buffer buf :: _ ->
        let a = Buffer.to_float buf in
        let result = Op.apply operator a b in
        let buf = Buffer.new_result result in
        let stack = [`Buffer buf] in
        let display = Stack.top_display stack in
        {cal with display; stack}
    | _ ->
        cal )


let input cal = function
  | `Cancel ->
      let cal = cancel cal in
      {cal with ac = true}
  | `Num n ->
      let cal = append_number cal n in
      {cal with ac = false}
  | `Dot ->
      let cal = append_dot cal in
      {cal with ac = false}
  | `Negate ->
      negate cal
  | #Op.t as op ->
      let cal = append_op cal op in
      {cal with last_operation = None}
  | `Eq ->
      eq cal