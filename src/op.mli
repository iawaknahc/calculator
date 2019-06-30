type t = Types.op

val ( >= ) : t -> t -> bool

val apply : t -> float -> float -> float

val of_button : Button.t -> t
