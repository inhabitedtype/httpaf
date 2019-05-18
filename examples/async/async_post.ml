open Core
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
    let headers =
      Headers.of_list
      [ "transfer-encoding", "chunked"
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
    let stdin = Lazy.force Reader.stdin in
    don't_wait_for (
      Reader.read_one_chunk_at_a_time stdin ~handle_chunk:(fun bs ~pos:off ~len ->
        Body.write_bigstring request_body bs ~off ~len;
        Body.flush request_body (fun () -> ());
        return (`Consumed(len, `Need_unknown)))
      >>| function
        | `Eof_with_unconsumed_data s -> Body.write_string request_body s; Body.close_writer request_body
        | `Eof                        -> Body.close_writer request_body
        | `Stopped ()                 -> assert false);
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
