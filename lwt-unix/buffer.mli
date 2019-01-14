type t

val create : int -> t

val get : t -> f:(Lwt_bytes.t -> off:int -> len:int -> int) -> int
val put : t -> f:(Lwt_bytes.t -> off:int -> len:int -> int Lwt.t) -> int Lwt.t
