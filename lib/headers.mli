type t

type name = string
type value = string

(** Case-insensitive equality for testing header names or values *)
val ci_equal : string -> string -> bool

val empty : t

val of_list     : (name * value) list -> t
val of_rev_list : (name * value) list -> t
val to_list     : t -> (name * value) list
val to_rev_list : t -> (name * value) list

val add               : t -> name -> value -> t
val add_unless_exists : t -> name -> value -> t
val add_list          : t -> (name * value) list -> t
val add_multi         : t -> (name * value list) list -> t

val remove  : t -> name -> t
val replace : t -> name -> value -> t

val mem       : t -> name -> bool
val get       : t -> name -> value option
val get_exn   : t -> name -> value
val get_multi : t -> name -> value list

val iter : f:(name -> value -> unit) -> t -> unit
val fold : f:(name -> value -> 'a -> 'a) -> init:'a -> t -> 'a

val to_string : t -> string
val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
