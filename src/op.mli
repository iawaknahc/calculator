type t =
  [ `Div
  | `Mul
  | `Sub
  | `Add ]

val ( >= ) : t -> t -> bool

val apply : t -> float -> float -> float
