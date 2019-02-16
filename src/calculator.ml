type t =
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


let reset cal =
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
            Misc.format_float f
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
            Misc.format_float f
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
  then reset cal
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
      let stack, last_operation = Stack.eq cal.stack in
      let display = Stack.top_display stack in
      {cal with display; stack; last_operation}
  | Some last_operation ->
      let stack = Stack.repeat cal.stack last_operation in
      let display = Stack.top_display stack in
      {cal with display; stack}


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


let string_to_buttons str =
  str
  |> String.to_char_list
  |> List.filter (fun c -> c <> ' ')
  |> List.map Button.of_char


let eval str =
  let button_list = string_to_buttons str in
  let cal = make () in
  let cal = List.fold_left input cal button_list in
  cal.display


let%test _ = eval "" = "0"

let%test _ = eval "1" = "1"

let%test _ = eval "12" = "12"

let%test _ = eval "12+" = "12"

let%test _ = eval "12+3" = "3"

let%test _ = eval "12+3*" = "3"

let%test _ = eval "12+3*5" = "5"

let%test _ = eval "12+3*5/" = "15"

let%test _ = eval "12+3*5/+" = "27"

let%test _ = eval "12+3*5/+=" = "54"

let%test _ = eval "=" = "0"

let%test _ = eval "==" = "0"

let%test _ = eval "==1" = "1"

let%test _ = eval "==1==" = "1"

let%test _ = eval "==1==+" = "1"

let%test _ = eval "==1==+=" = "2"

let%test _ = eval "==1==+==" = "3"

let%test _ = eval "==1==+==6" = "6"

let%test _ = eval "==1==+==6=" = "7"

let%test _ = eval "1+2C3=" = "4"

let%test _ = eval "1+2C3==" = "7"

let%test _ = eval "1+2+C=" = "3"

let%test _ = eval "1+2+C==" = "3"

let%test _ = eval "1+2+C.2=" = "3.2"

let%test _ = eval "1+2+C.2==" = "3.4"

let%test _ = eval "1+2+C.2==~" = "-3.4"

let%test _ = eval "1+2+C.2==~=" = "-3.2"
