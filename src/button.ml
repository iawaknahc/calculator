open Types

type t = button

let of_string = function
  | "c" | "C" -> Button_Cancel
  | "~" -> Button_Negate
  | "/" -> Button_Div
  | "*" -> Button_Mul
  | "-" -> Button_Sub
  | "+" -> Button_Add
  | "=" -> Button_Eq
  | "%" -> Button_Percent
  | "." -> Button_Dot
  | "0" -> Button_Num 0
  | "1" -> Button_Num 1
  | "2" -> Button_Num 2
  | "3" -> Button_Num 3
  | "4" -> Button_Num 4
  | "5" -> Button_Num 5
  | "6" -> Button_Num 6
  | "7" -> Button_Num 7
  | "8" -> Button_Num 8
  | "9" -> Button_Num 9
  | _ -> failwith "invalid char"
