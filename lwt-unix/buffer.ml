open Lwt.Infix

(* Based on the Buffer module in httpaf_async.ml. *)
type t =
  { buffer      : Lwt_bytes.t
  ; mutable off : int
  ; mutable len : int }

let create size =
  let buffer = Lwt_bytes.create size in
  { buffer; off = 0; len = 0 }

let compress t =
  if t.len = 0
  then begin
    t.off <- 0;
    t.len <- 0;
  end else if t.off > 0
  then begin
    Lwt_bytes.blit t.buffer t.off t.buffer 0 t.len;
    t.off <- 0;
  end

let get t ~f =
  let n = f t.buffer ~off:t.off ~len:t.len in
  t.off <- t.off + n;
  t.len <- t.len - n;
  if t.len = 0
  then t.off <- 0;
  n

let put t ~f =
  compress t;
  f t.buffer ~off:(t.off + t.len) ~len:(Lwt_bytes.length t.buffer - t.len)
  >>= fun n ->
  t.len <- t.len + n;
  Lwt.return n
