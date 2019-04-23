open Base
open Lwt.Infix
module Arg = Caml.Arg

open Httpaf
open Httpaf_lwt_unix

let error_handler _ = assert false

let main port host =
  Lwt_unix.getaddrinfo host (Int.to_string port) [Unix.(AI_FAMILY PF_INET)]
  >>= fun addresses ->
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect socket (List.hd_exn addresses).Unix.ai_addr
  >>= fun () ->
  let finished, notify_finished = Lwt.wait () in
  let response_handler =
    Httpaf_examples.Client.print ~on_eof:(Lwt.wakeup_later notify_finished)
  in
  let headers = Headers.of_list [ "host", host ] in
  let request_body =
      Client.TLS.request
      ~error_handler
      ~response_handler
      socket
      (Request.create ~headers `GET "/")
  in
  Body.close_writer request_body;
  finished
;;
