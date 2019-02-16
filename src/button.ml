(* TODO: Implement percent *)
type t =
  [ `Cancel
  | `Negate
  | `Div
  | `Mul
  | `Sub
  | `Add
  | `Eq
  | `Dot
  | `Num of int ]

let of_char = function
  | 'c' | 'C' ->
      `Cancel
  | '~' ->
      `Negate
  | '/' ->
      `Div
  | '*' ->
      `Mul
  | '-' ->
      `Sub
  | '+' ->
      `Add
  | '=' ->
      `Eq
  | '.' ->
      `Dot
  | '0' .. '9' as ch ->
      `Num (Char.code ch - Char.code '0')
  | _ ->
      failwith "invalid char"
