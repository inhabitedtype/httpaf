open Base
open Lwt.Infix
module Arg = Caml.Arg

open Httpaf
open Httpaf_lwt_unix

let error_handler _ = assert false

let main port host =
  Lwt_io.(read stdin)
  >>= fun body ->
  Lwt_unix.getaddrinfo host (Int.to_string port) [Unix.(AI_FAMILY PF_INET)]
  >>= fun addresses ->
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect socket (List.hd_exn addresses).Unix.ai_addr
  >>= fun () ->
  let finished, notify_finished = Lwt.wait () in
  let response_handler =
    Httpaf_examples.Client.print ~on_eof:(Lwt.wakeup_later notify_finished)
  in
  let headers =
    Headers.of_list
    [ "content-length"   , (Int.to_string (String.length body))
    ; "connection"       , "close"
    ; "host"             , host
    ]
  in
  let request_body =
    Client.request
      ~error_handler
      ~response_handler
      socket
      (Request.create ~headers `POST "/")
  in
  Body.write_string request_body body;
  Body.close_writer request_body;
  finished
;;

let () =
  let host = ref None in
  let port = ref 8080 in

  Arg.parse
    ["-p", Set_int port, " Port number (8080 by default)"]
    (fun host_argument -> host := Some host_argument)
    "lwt_get.exe [-p N] HOST";
  let host =
    match !host with
    | None -> failwith "No hostname provided"
    | Some host -> host
  in
  Lwt_main.run (main !port host)
;;
