(*----------------------------------------------------------------------------
    Copyright (c) 2017-2019 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

module Reader = Parse.Reader
module Writer = Serialize.Writer

module Oneshot = struct
  type error =
    [ `Malformed_response of string | `Invalid_response_body_length of Response.t | `Exn of exn ]

  type response_handler = Response.t -> Body.Reader.t  -> unit
  type error_handler = error -> unit

  type state =
    | Awaiting_response
    | Received_response of Response.t * Body.Reader.t
    | Closed

  type t =
    { request          : Request.t
    ; request_body     : Body.Writer.t
    ; error_handler    : (error -> unit)
    ; reader : Reader.response
    ; writer : Writer.t
    ; state  : state ref
    ; mutable error_code : [ `Ok | error ]
    }

  let request ?(config=Config.default) request ~error_handler ~response_handler =
    let state = ref Awaiting_response in
    let request_method = request.Request.meth in
    let handler response body =
      state := Received_response(response, body);
      response_handler response body
    in
    let writer = Writer.create () in
    let request_body =
      let encoding =
        match Request.body_length request with
        | `Fixed _ | `Chunked as encoding -> encoding
        | `Error `Bad_request ->
          failwith "Httpaf.Client_connection.request: invalid body length"
      in
      Body.Writer.create (Bigstringaf.create config.request_body_buffer_size)
        ~encoding ~writer
    in
    let t =
      { request
      ; request_body
      ; error_handler
      ; error_code = `Ok
      ; reader = Reader.response ~request_method handler
      ; writer
      ; state }
    in
    Writer.write_request t.writer request;
    request_body, t
  ;;

  let flush_request_body t =
    if Body.Writer.has_pending_output t.request_body
    then Body.Writer.transfer_to_writer t.request_body
  ;;

  let set_error_and_handle_without_shutdown t error =
    t.state := Closed;
    t.error_code <- (error :> [`Ok | error]);
    t.error_handler error;
  ;;

  let unexpected_eof t =
    set_error_and_handle_without_shutdown t (`Malformed_response "unexpected eof");
  ;;

  let shutdown_reader t =
    Reader.force_close t.reader;
    begin match !(t.state) with
    | Awaiting_response -> unexpected_eof t;
    | Closed -> ()
    | Received_response(_, response_body) ->
      Body.Reader.close response_body;
      Body.Reader.execute_read response_body;
    end;
  ;;

  let shutdown_writer t =
    flush_request_body t;
    Writer.close t.writer;
    Body.Writer.close t.request_body;
  ;;

  let shutdown t =
    shutdown_reader t;
    shutdown_writer t;
  ;;

  let set_error_and_handle t error =
    Reader.force_close t.reader;
    begin match !(t.state) with
    | Closed -> ()
    | Awaiting_response ->
      set_error_and_handle_without_shutdown t error;
    | Received_response(_, response_body) ->
      Body.Reader.close response_body;
      Body.Reader.execute_read response_body;
      set_error_and_handle_without_shutdown t error;
    end
  ;;

  let report_exn t exn =
    set_error_and_handle t (`Exn exn)
  ;;

  let flush_response_body t =
    match !(t.state) with
    | Awaiting_response | Closed -> ()
    | Received_response(_, response_body) ->
      try Body.Reader.execute_read response_body
      with exn -> report_exn t exn
  ;;

  let _next_read_operation t =
    match !(t.state) with
    | Awaiting_response | Closed -> Reader.next t.reader
    | Received_response(_, response_body) ->
      if not (Body.Reader.is_closed response_body)
      then Reader.next t.reader
      else begin
        Reader.force_close t.reader;
        Reader.next        t.reader
      end
  ;;

  let next_read_operation t =
    match _next_read_operation t with
    | `Error (`Parse(marks, message)) ->
      let message = String.concat "" [ String.concat ">" marks; ": "; message] in
      set_error_and_handle t (`Malformed_response message);
      `Close
    | `Error (`Invalid_response_body_length _ as error) ->
      set_error_and_handle t error;
      `Close
    | (`Read | `Close) as operation -> operation
  ;;

  let read_with_more t bs ~off ~len more =
    let consumed = Reader.read_with_more t.reader bs ~off ~len more in
    flush_response_body t;
    consumed
  ;;

  let read t bs ~off ~len =
    read_with_more t bs ~off ~len Incomplete

  let read_eof t bs ~off ~len =
    let bytes_read = read_with_more t bs ~off ~len Complete in
    begin match !(t.state) with
    | Received_response _ | Closed -> ()
    | Awaiting_response -> unexpected_eof t;
    end;
    bytes_read
  ;;

  let next_write_operation t =
    flush_request_body t;
    if Body.Writer.is_closed t.request_body
    (* Even though we've just done [flush_request_body], it might still be the case that
       [Body.Writer.has_pending_output] returns true, because it does so when
       we've written all output except for the final chunk. *)
    && not (Body.Writer.has_pending_output t.request_body)
    then Writer.close t.writer;
    Writer.next t.writer
  ;;

  let yield_writer t k =
    if Body.Writer.is_closed t.request_body
    && not (Body.Writer.has_pending_output t.request_body)
    then begin
      Writer.close t.writer;
      k ()
    end else
      Writer.on_wakeup t.writer k

  let report_write_result t result =
    Writer.report_result t.writer result

  let is_closed t = Reader.is_closed t.reader && Writer.is_closed t.writer
end
