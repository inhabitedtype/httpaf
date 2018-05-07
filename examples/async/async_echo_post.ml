open Core
open Async

open Httpaf
open Httpaf_async


let error_handler _ ?request error start_response =
  let response_body = start_response Headers.empty in
  begin match error with
  | `Exn exn ->
    Body.write_string response_body (Exn.to_string exn);
    Body.write_string response_body "\n";
  | #Status.standard as error ->
    Body.write_string response_body (Status.default_reason_phrase error)
  end;
  Body.close_writer response_body
;;

let request_handler _ reqd =
  match Reqd.request reqd  with
  | { Request.meth = `POST; headers } ->
    let response =
      let content_type =
        match Headers.get headers "content-type" with
        | None   -> "application/octet-stream"
        | Some x -> x
      in
      Response.create ~headers:(Headers.of_list ["content-type", content_type; "connection", "close"]) `OK
    in
    let request_body  = Reqd.request_body reqd in
    let response_body = Reqd.respond_with_streaming reqd response in
    let rec on_read buffer ~off ~len =
      Body.write_bigstring response_body buffer ~off ~len;
      Body.schedule_read request_body ~on_eof ~on_read;
    and on_eof () =
      print_endline "eof";
      Body.close_writer response_body
    in
    Body.schedule_read (Reqd.request_body reqd) ~on_eof ~on_read
  | _ -> Reqd.respond_with_string reqd (Response.create `Method_not_allowed) ""
;;

let main port max_accepts_per_batch () =
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  Tcp.(Server.create_sock ~on_handler_error:`Raise
      ~backlog:10_000 ~max_connections:10_000 ~max_accepts_per_batch where_to_listen)
    (Server.create_connection_handler ~request_handler ~error_handler)
  >>= fun server ->
  Deferred.never ()

let () =
  Command.async_spec
    ~summary:"Start a hello world Async server"
    Command.Spec.(empty +>
      flag "-p" (optional_with_default 8080 int)
        ~doc:"int Source port to listen on"
      +>
      flag "-a" (optional_with_default 1 int)
        ~doc:"int Maximum accepts per batch"
    ) main
  |> Command.run
