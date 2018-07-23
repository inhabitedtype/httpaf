open Httpaf
open Httpaf_lwt_unix
open Lwt.Infix

let error_handler _ ?request:_ error start_response =
  let response_body = start_response Headers.empty in
  match error with
  | `Exn exn ->
    Body.write_string response_body (Printexc.to_string exn);
    Body.write_string response_body "\n"
  | #Status.standard as error ->
    Body.write_string response_body (Status.default_reason_phrase error);
  Body.close_writer response_body

let request_handler _ reqd =
  match Reqd.request reqd with
  | {Request.meth = `POST; headers; _} ->
    let response =
      let content_type =
        match Headers.get headers "content-type" with
        | None -> "application/octet-stream"
        | Some x -> x in
      let headers = Headers.of_list [
        "content-type", content_type;
        "connection", "close";
      ] in
      Response.create ~headers `OK in
    let request_body = Reqd.request_body reqd in
    let response_body = Reqd.respond_with_streaming reqd response in
    let
      rec on_read buffer ~off ~len =
        Body.write_bigstring response_body buffer ~off ~len;
        Body.schedule_read request_body ~on_eof ~on_read
      and on_eof () =
        print_endline "eof";
        Body.close_writer response_body
    in
    Body.schedule_read (Reqd.request_body reqd) ~on_eof ~on_read
  | _ ->
    Reqd.respond_with_string reqd (Response.create `Method_not_allowed) ""

let main port =
  let sockaddr = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let handler = Server.create_connection_handler ~error_handler ~request_handler in
  Lwt.async begin fun () ->
    Lwt_io.establish_server_with_client_socket ~backlog:10_000 sockaddr handler
      >|= ignore
  end;
  fst (Lwt.wait ())

let () =
  let port = ref 8080 in
  Arg.parse
    ["-p", Arg.Set_int port, " Port number to listen on."]
    (fun _ ->
      prerr_endline "No posititonal arguments accepted.";
      exit 64)
    "lwt_unix_echo_post [-p PORT] [-a N-ACCEPT-PER-BATCH]";
  Lwt_main.run (main !port)
