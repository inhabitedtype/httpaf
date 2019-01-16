open Core
open Async

type t

val create   : int -> t

val get : t -> f:(Bigstring.t -> off:int -> len:int -> int) -> int
val put : t -> f:(Bigstring.t -> off:int -> len:int -> int) -> int
val put_async : t -> f:(Bigstring.t -> off:int -> len:int -> int Deferred.t) -> int Deferred.t
