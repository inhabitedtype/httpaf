open Core
open Async

open Httpaf_async

let request_handler (_ : Socket.Address.Inet.t) = Httpaf_examples.Server.upgrade
let error_handler (_ : Socket.Address.Inet.t) = Httpaf_examples.Server.error_handler

let upgrade_handler (_ : Socket.Address.Inet.t) reader writer =
  Reader.read_one_chunk_at_a_time reader ~handle_chunk:(fun bigstring ~pos ~len ->
    Writer.write_bigstring writer bigstring ~pos ~len;
    return `Continue)
  >>| function
  | `Eof | `Stopped _ | `Eof_with_unconsumed_data _ -> ()
;;

let main port max_accepts_per_batch () =
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  Tcp.(Server.create_sock ~on_handler_error:`Raise
      ~backlog:10_000 ~max_connections:10_000 ~max_accepts_per_batch where_to_listen)
    (Server.create_connection_handler
       ~request_handler
       ~error_handler
       ~upgrade_handler:(Some upgrade_handler))
  >>= fun _server ->
    Stdio.printf "Listening on port %i, upgrading, and echoing data.\n" port;
    Stdio.printf "To send an interactive upgrade request, try\n\n";
    Stdio.printf "  examples/script/upgrade-connect\n%!";
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
