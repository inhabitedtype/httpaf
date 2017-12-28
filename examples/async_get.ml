open Core
open Async

open Httpaf
open Httpaf_async

let response_handler finished response response_body =
  match response with
  | { Response.status = `OK; _ } ->
    let rec on_read bs ~off ~len =
      Bigstring.to_string ~off ~len bs |> print_endline;
      Body.schedule_read response_body ~on_read ~on_eof
    and on_eof () = Ivar.fill finished () in
    Body.schedule_read response_body ~on_read ~on_eof;
;;

let error_handler _ = assert false

let main port address () =
  let where_to_connect = Tcp.to_host_and_port address port in
  let finished = Ivar.create () in
  Tcp.connect_sock where_to_connect
  >>= fun socket ->
    let request_body =
      Client.request
        ~error_handler
        ~response_handler:(response_handler finished)
        socket
        (Request.create `GET "/")
    in
    Body.close request_body;
    Ivar.read finished
;;

let () =
  Command.async
    ~summary:"Start a hello world Async server"
    Command.Spec.(empty +>
      flag "-p" (optional_with_default 80 int)
        ~doc:"int destination port"
      +>
      flag "-a" (required string)
        ~doc:"string destination ip"
    ) main
  |> Command.run
