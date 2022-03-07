open Base
open Lwt.Infix
module Arg = Caml.Arg

open Httpaf_lwt_unix

let request_handler (_ : Unix.sockaddr) = Httpaf_examples.Server.upgrade
let error_handler (_ : Unix.sockaddr) = Httpaf_examples.Server.error_handler

let upgrade_handler (_ : Unix.sockaddr) (fd : Lwt_unix.file_descr) =
  let input = Lwt_io.of_fd fd ~mode:Input in
  let output = Lwt_io.of_fd fd ~mode:Output in
  let rec loop () =
    Lwt_io.read input ~count:4096
    >>= fun data ->
    Lwt_io.write output data
    >>= fun () ->
    loop ()
  in
  loop ()
;;

let main port =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt.async (fun () ->
    Lwt_io.establish_server_with_client_socket
      listen_address
      (Server.create_connection_handler
         ~request_handler
         ~error_handler
         ~upgrade_handler:(Some upgrade_handler))
    >|= fun _server ->
      Stdio.printf "Listening on port %i, upgrading, and echoing data.\n" port;
      Stdio.printf "To send an interactive upgrade request, try\n\n";
      Stdio.printf "  examples/script/upgrade-connect\n%!");
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
;;

let () =
  let port = ref 8080 in
  Arg.parse
    ["-p", Arg.Set_int port, " Listening port number (8080 by default)"]
    ignore
    "Echoes POST requests. Runs forever.";
  main !port
;;

