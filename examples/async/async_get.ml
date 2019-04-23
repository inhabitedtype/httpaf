open! Core
open Async

open Httpaf
open Httpaf_async

let error_handler _ = assert false

let main port host () =
  let where_to_connect = Tcp.Where_to_connect.of_host_and_port { host; port } in
  Tcp.connect_sock where_to_connect
  >>= fun socket ->
    let finished = Ivar.create () in
    let response_handler = Httpaf_examples.Client.print ~on_eof:(Ivar.fill finished) in
    let headers = Headers.of_list [ "host", host ] in
    let request_body =
      Client.request
        ~error_handler
        ~response_handler
        socket
        (Request.create ~headers `GET "/")
    in
    Body.close_writer request_body;
    Ivar.read finished
;;

let () =
  Command.async
    ~summary:"Start a hello world Async client"
    Command.Param.(
      map (both
          (flag "-p" (optional_with_default 80 int)
            ~doc:"int destination port")
          (anon ("host" %: string)))
        ~f:(fun (port, host) ->
              (fun () -> main port host ())))
  |> Command.run
