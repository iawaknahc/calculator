type t =
  [ `Cancel
  | `Negate
  | `Div
  | `Mul
  | `Sub
  | `Add
  | `Eq
  | `Percent
  | `Dot
  | `Num of int ]

let of_string = function
  | "c" | "C" ->
      `Cancel
  | "~" ->
      `Negate
  | "/" ->
      `Div
  | "*" ->
      `Mul
  | "-" ->
      `Sub
  | "+" ->
      `Add
  | "=" ->
      `Eq
  | "%" ->
      `Percent
  | "." ->
      `Dot
  | "0" ->
      `Num 0
  | "1" ->
      `Num 1
  | "2" ->
      `Num 2
  | "3" ->
      `Num 3
  | "4" ->
      `Num 4
  | "5" ->
      `Num 5
  | "6" ->
      `Num 6
  | "7" ->
      `Num 7
  | "8" ->
      `Num 8
  | "9" ->
      `Num 9
  | _ ->
      failwith "invalid char"
