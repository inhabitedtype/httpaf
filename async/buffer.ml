(** XXX(seliopou): Replace Angstrom.Buffered with a module like this, while
    also supporting growing the buffer. Clients can use this to buffer and the
    use the unbuffered interface for actually running the parser. *)
open Core
open Async

type t =
  { buffer      : Bigstring.t
  ; mutable off : int
  ; mutable len : int }

let create size =
  let buffer = Bigstring.create size in
  { buffer; off = 0; len = 0 }
;;

let compress t =
  if t.len = 0
  then begin
    t.off <- 0;
    t.len <- 0;
  end else if t.off > 0
  then begin
    Bigstring.blit ~src:t.buffer ~src_pos:t.off ~dst:t.buffer ~dst_pos:0 ~len:t.len;
    t.off <- 0;
  end
;;

let get t ~f =
  let n = f t.buffer ~off:t.off ~len:t.len in
  t.off <- t.off + n;
  t.len <- t.len - n;
  if t.len = 0
  then t.off <- 0;
  n
;;

let put t ~f =
  compress t;
  let n = f t.buffer ~off:(t.off + t.len) ~len:(Bigstring.length t.buffer - t.len) in
  t.len <- t.len + n;
  n
;;

let put_async t ~f =
  compress t;
  f t.buffer ~off:(t.off + t.len) ~len:(Bigstring.length t.buffer - t.len)
  >>= fun n ->
  t.len <- t.len + n;
  Deferred.return n
;;

