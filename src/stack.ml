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
  let ops = List.filter_map (function `Op op -> Some op | _ -> None) stack in
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


let eq stack =
  (*
     * The behavior of eq depends on the top of stack is operator or not.
     * If it is an operand, just eval.
     * If it is an operator, try preview-eval.
     *   If success, the stack becomes [result; operator; result] and eval this stack.
     *   Otherwise, add missing operand and eval the stack.
     * *)
  match stack with
  | `Buffer _ :: _ ->
      let last_operation = last_operation stack in
      let stack = simplify stack Op.max_precedence in
      let result = eval stack in
      let buf = Buffer.new_result result in
      ([`Buffer buf], last_operation)
  | `Op op :: _ ->
    ( match preview stack with
    | None ->
        let stack = add_missing_operand stack in
        let last_operation = last_operation stack in
        let stack = simplify stack Op.max_precedence in
        let result = eval stack in
        let buf = Buffer.new_result result in
        ([`Buffer buf], last_operation)
    | Some f ->
        let buf = Buffer.new_result f in
        let stack = [`Buffer buf; `Op op; `Buffer buf] in
        let last_operation = last_operation stack in
        let stack = simplify stack Op.max_precedence in
        let result = eval stack in
        let buf = Buffer.new_result result in
        ([`Buffer buf], last_operation) )
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
