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
