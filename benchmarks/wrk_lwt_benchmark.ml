open Base
open Httpaf_lwt_unix
module Arg = Caml.Arg

let main port =
  let open Lwt.Infix in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  let request_handler _ = Httpaf_examples.Server.benchmark in
  let error_handler _ = Httpaf_examples.Server.error_handler in
  Lwt.async begin fun () ->
    Lwt_io.establish_server_with_client_socket
      ~backlog:11_000
      listen_address
      (Server.create_connection_handler ~request_handler ~error_handler)
    >>= fun _server -> Lwt.return_unit
  end;
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
;;

let () =
  let port = ref 8080 in
  Arg.parse
    ["-p", Arg.Set_int port, " Listening port number (8080 by default)"]
    ignore
    "Responds to requests with a fixed string for benchmarking purposes.";
  main !port
;;
