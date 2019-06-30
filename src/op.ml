open Types

type t = op

let precedence = function Op_Div | Op_Mul -> 10 | Op_Sub | Op_Add -> 9

let ( >= ) op1 op2 = precedence op1 >= precedence op2

let apply op a b =
  match op with
  | Op_Div -> if b = 0.0 then nan else a /. b
  | Op_Mul -> a *. b
  | Op_Sub -> a -. b
  | Op_Add -> a +. b

let of_button = function
  | Button_Div -> Op_Div
  | Button_Mul -> Op_Mul
  | Button_Add -> Op_Add
  | Button_Sub -> Op_Sub
  | _ -> failwith "invalid button"
