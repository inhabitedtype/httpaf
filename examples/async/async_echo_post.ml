open Core
open Async

open Httpaf_async

let request_handler (_ : Socket.Address.Inet.t) = Httpaf_examples.Server.echo_post
let error_handler (_ : Socket.Address.Inet.t) = Httpaf_examples.Server.error_handler

let main port max_accepts_per_batch () =
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  Tcp.(Server.create_sock ~on_handler_error:`Raise
      ~backlog:10_000 ~max_connections:10_000 ~max_accepts_per_batch where_to_listen)
    (Server.create_connection_handler ~request_handler ~error_handler)
  >>= fun _server ->
    Stdio.printf "Listening on port %i and echoing POST requests.\n" port;
    Stdio.printf "To send a POST request, try one of the following\n\n";
    Stdio.printf "  echo \"Testing echo POST\" | dune exec examples/async/async_post.exe\n";
    Stdio.printf "  echo \"Testing echo POST\" | dune exec examples/lwt/lwt_post.exe\n";
    Stdio.printf "  echo \"Testing echo POST\" | curl -XPOST --data @- http://localhost:%d\n\n%!" port;
    Deferred.never ()
;;

let () =
  Command.async
    ~summary:"Echo POST requests"
    Command.Param.(
      map (both
          (flag "-p" (optional_with_default 8080 int)
            ~doc:"int Source port to listen on")
          (flag "-a" (optional_with_default 1 int)
            ~doc:"int Maximum accepts per batch"))
        ~f:(fun (port, accepts) ->
              (fun () -> main port accepts ())))
  |> Command.run
;;
