open Base
open Lwt.Infix
module Arg = Caml.Arg

open Httpaf_lwt_unix

let request_handler (_ : Unix.sockaddr) = Httpaf_examples.Server.echo_post
let error_handler (_ : Unix.sockaddr) = Httpaf_examples.Server.error_handler

let main port =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  let certfile = "./certificates/server.pem" in
  let keyfile = "./certificates/server.key" in
  Lwt.async (fun () ->
    Lwt_io.establish_server_with_client_socket
      listen_address
      (Server.TLS.create_connection_handler
        ?server:None
        ~certfile
        ~keyfile
        ?config:None
        ~request_handler
        ~error_handler)
    >|= fun _server ->
      Stdio.printf "Listening on port %i and echoing POST requests.\n" port;
      Stdio.printf "To send a POST request, try one of the following\n\n";
      Stdio.printf "  echo \"Testing echo POST\" | dune exec examples/async/async_post.exe\n";
      Stdio.printf "  echo \"Testing echo POST\" | dune exec examples/lwt/lwt_post.exe\n";
      Stdio.printf "  echo \"Testing echo POST\" | curl -XPOST --data @- http://localhost:8080\n\n%!");
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
