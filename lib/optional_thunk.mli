type t

val none : t
val some : (unit -> unit) -> t

val is_none : t -> bool
val is_some : t -> bool

val call_if_some : t -> unit
val unchecked_value : t -> unit -> unit
