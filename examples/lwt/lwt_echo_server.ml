(* TODO This needs to be paired with the requester example. *)
(* TODO Usage to comment. *)

let connection_handler : Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t =
  let module Body = Httpaf.Body in
  let module Headers = Httpaf.Headers in
  let module Reqd = Httpaf.Reqd in
  let module Response = Httpaf.Response in
  let module Status = Httpaf.Status in

  let request_handler : Unix.sockaddr -> _ Reqd.t -> unit =
      fun _client_address request_descriptor ->

    let request = Reqd.request request_descriptor in
    match request.meth with
    | `POST ->
      let request_body = Reqd.request_body request_descriptor in

      let response_content_type =
        match Headers.get request.headers "Content-Type" with
        | Some request_content_type -> request_content_type
        | None -> "application/octet-stream"
      in

      (* Due to a possible bug in http/af, read from the body only once, and
         create the response based on the data in that first read.

         The bug is (possibly) in the client. Client_connection seems to go into
         a read loop despite Client_connection.shutdown being called, due to the
         reader being in the Partial state, and the next operation function
         unconditionally returning `Read in that case.

         One workaround for this is to have the server send a Content-Length
         header. To do that, this code has the server simply reply after the
         first chunk is read, and use that chunk's length.

         The code I would expect to work, without the possible bug, is commented
         out below. *)

      (* Body.schedule_read
        request_body
        ~on_eof:ignore
        ~on_read:(fun request_data ~off ~len ->
          let response =
            Response.create
              ~headers:(Headers.of_list [
                "Content-Type", response_content_type;
                "Content-Length", string_of_int len;
                "Connection", "close";
              ])
              `OK
          in

          let response_body =
            Reqd.respond_with_streaming request_descriptor response in

          Body.write_bigstring response_body request_data ~off ~len;
          Body.close_writer response_body) *)

      let response =
        Response.create
          ~headers:(Headers.of_list [
            "Content-Type", response_content_type;
            "Connection", "close";
          ])
          `OK
      in

      let response_body =
        Reqd.respond_with_streaming request_descriptor response in

      let rec respond () =
        Body.schedule_read
          request_body
          ~on_eof:(fun () -> Body.close_writer response_body)
          ~on_read:(fun request_data ~off ~len ->
            Body.write_bigstring response_body request_data ~off ~len;
            respond ())
      in
      respond ()

    | _ ->
      Reqd.respond_with_string
        request_descriptor (Response.create `Method_not_allowed) ""
  in

  let error_handler :
      Unix.sockaddr ->
      ?request:Httpaf.Request.t ->
      _ ->
      (Headers.t -> [`write] Body.t) ->
        unit =
      fun _client_address ?request:_ error start_response ->

    let response_body = start_response Headers.empty in

    begin match error with
    | `Exn exn ->
      Body.write_string response_body (Printexc.to_string exn);
      Body.write_string response_body "\n";

    | #Status.standard as error ->
      Body.write_string response_body (Status.default_reason_phrase error)
    end;

    Body.close_writer response_body
  in

  Httpaf_lwt.Server.create_connection_handler
    ?config:None
    ~request_handler
    ~error_handler



let () =
  let open Lwt.Infix in

  let port = ref 8080 in
  Arg.parse
    ["-p", Arg.Set_int port, " Listening port number (8080 by default)"]
    ignore
    "Echoes POST requests. Runs forever.";

  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, !port)) in

  Lwt.async begin fun () ->
    Lwt_io.establish_server_with_client_socket
      listen_address connection_handler
    >>= fun _server ->
    Lwt.return_unit
  end;

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
