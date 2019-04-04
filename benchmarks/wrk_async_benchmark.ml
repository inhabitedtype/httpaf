open Core
open Async
open Httpaf_async

let main port max_accepts_per_batch () =
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  let request_handler _ = Httpaf_examples.Server.benchmark in
  let error_handler _ = Httpaf_examples.Server.error_handler in
  Tcp.(Server.create_sock ~on_handler_error:`Ignore
      ~backlog:11_000 ~max_connections:10_000 ~max_accepts_per_batch where_to_listen)
    (Server.create_connection_handler ~request_handler ~error_handler)
  >>= fun server ->
  Deferred.forever () (fun () ->
    Clock.after Time.Span.(of_sec 0.5) >>| fun () ->
      Log.Global.printf "conns: %d" (Tcp.Server.num_connections server));
  Deferred.never ()

let () =
  Command.async
    ~summary:"Start a hello world Async server"
    Command.Param.(
      map (both
          (flag "-p" (optional_with_default 8080 int)
            ~doc:"int Source port to listen on")
          (flag "-a" (optional_with_default 1 int)
            ~doc:"int Maximum accepts per batch"))
        ~f:(fun (port, accepts) ->
              (fun () -> main port accepts ())))
  |> Command.run
