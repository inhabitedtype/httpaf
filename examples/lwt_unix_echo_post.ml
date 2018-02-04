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

let main port max_accepts_per_batch =
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let sockaddr = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  Lwt_unix.bind sock sockaddr >>= fun () ->
  Lwt_unix.listen sock 10_000;
  let h = Server.create_connection_handler ~error_handler ~request_handler in
  let rec serve () =
    Lwt_unix.accept_n sock max_accepts_per_batch >>= fun (accepts, exn) ->
    begin match exn with
    | None -> ()
    | Some exn -> prerr_endline ("Accept failed: " ^ Printexc.to_string exn)
    end;
    List.iter (fun (sa, fd) -> Lwt.async (fun () -> h fd sa)) accepts;
    serve () in
  serve ()

let () =
  let port = ref 8080 in
  let batch_capacity = ref 100 in
  Arg.parse
    ["-p", Arg.Set_int port, " Port number to listen on.";
     "-a", Arg.Set_int batch_capacity, " Maximum number of accepts per batch."]
    (fun _ ->
      prerr_endline "No posititonal arguments accepted.";
      exit 64)
    "lwt_unix_echo_post [-p PORT] [-a N-ACCEPT-PER-BATCH]";
  Lwt_main.run (main !port !batch_capacity)
