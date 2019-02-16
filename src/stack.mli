type t

val empty : t

val top_display : t -> string

val append_number : t -> max:int -> n:int -> t

val append_dot : t -> t

val append_op : t -> Op.t -> t

val negate : t -> t

val cancel : t -> t

val eq : t -> t * (Op.t * float) option

val repeat : t -> Op.t * float -> t
