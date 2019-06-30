open Types

type t =
  { max_input_number_of_digits : int
  ; ac : bool
  ; display : string
  ; last_operation : (Op.t * float) option
  ; stack : Stack.t
  }

let make () =
  { max_input_number_of_digits = 9
  ; ac = true
  ; display = "0"
  ; last_operation = None
  ; stack = Stack.empty
  }


let reset _cal = make ()

let append_number cal n =
  let max = cal.max_input_number_of_digits in
  let stack = Stack.append_number cal.stack ~max ~n in
  let display = Stack.top_display stack in
  { cal with display; stack }


let append_dot cal =
  let stack = Stack.append_dot cal.stack in
  let display = Stack.top_display stack in
  { cal with display; stack }


let append_op cal op =
  let stack = Stack.append_op cal.stack op in
  let display = Stack.top_display stack in
  { cal with display; stack }


let negate cal =
  let stack = Stack.negate cal.stack in
  let display = Stack.top_display stack in
  { cal with display; stack }


let cancel cal =
  if cal.ac
  then reset cal
  else
    let stack = Stack.cancel cal.stack in
    let display = Stack.top_display stack in
    { cal with display; stack }


let eq cal =
  match cal.last_operation with
  | None ->
      let stack, last_operation = Stack.eq cal.stack in
      let display = Stack.top_display stack in
      { cal with display; stack; last_operation }
  | Some last_operation ->
      let stack = Stack.repeat cal.stack last_operation in
      let display = Stack.top_display stack in
      { cal with display; stack }


let percent cal =
  let stack = Stack.percent cal.stack in
  let display = Stack.top_display stack in
  { cal with display; stack }


let input cal = function
  | Button_Cancel ->
      let cal = cancel cal in
      { cal with ac = true }
  | Button_Num n ->
      let cal = append_number cal n in
      { cal with ac = false }
  | Button_Dot ->
      let cal = append_dot cal in
      { cal with ac = false }
  | Button_Negate ->
      negate cal
  | Button_Eq ->
      eq cal
  | Button_Percent ->
      percent cal
  | op ->
      let cal = append_op cal (Op.of_button op) in
      { cal with last_operation = None }


let button cal str = Button.of_string str |> input cal

let display cal = cal.display
