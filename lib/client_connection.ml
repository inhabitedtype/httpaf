(*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.

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

  type response_handler = Response.t -> [`read] Body.t  -> unit
  type error_handler = error -> unit

  type state =
    | Awaiting_response
    | Received_response of Response.t * [`read] Body.t
    | Closed

  type t =
    { request          : Request.t
    ; request_body     : [ `write ] Body.t
    ; response_handler : (Response.t -> [`read] Body.t -> unit)
    ; error_handler    : (error -> unit)
    ; reader : Reader.response
    ; writer : Writer.t
    ; state  : state ref
    ; mutable error_code : [ `Ok | error ]
    }

  let request request ~error_handler ~response_handler =
    let state = ref Awaiting_response in
    let request_method = request.Request.meth in
    let handler response body =
      state := Received_response(response, body);
      response_handler response body
    in
    (* XXX(seliopou): This buffer size should be configurable *)
    let request_body = Body.create (Bigstring.create 0x1000) in
    let t =
      { request
      ; request_body
      ; response_handler
      ; error_handler
      ; error_code = `Ok
      ; reader = Reader.response ~request_method handler
      ; writer = Writer.create ()
      ; state }
    in
    Writer.write_request t.writer request;
    request_body, t
  ;;

  let flush_request_body t =
    if Body.has_pending_output t.request_body
    then
      let encoding =
        match Request.body_length t.request with
        | `Fixed _ | `Chunked as encoding -> encoding
        | `Error _ -> assert false (* XXX(seliopou): This needs to be handled properly *)
      in
      Body.transfer_to_writer_with_encoding t.request_body ~encoding t.writer
  ;;

  let shutdown_reader t =
    Reader.close t.reader;
    begin match !(t.state) with
    | Awaiting_response | Closed -> ()
    | Received_response(_, response_body) ->
      Body.close_reader response_body;
      Body.execute_read response_body;
    end;
  ;;

  let shutdown_writer t =
    flush_request_body t;
    Writer.close t.writer;
    Body.close_writer t.request_body;
  ;;

  let shutdown t =
    shutdown_reader t;
    shutdown_writer t;
  ;;

  let set_error_and_handle t error =
    shutdown t;
    t.state := Closed;
    t.error_code <- (error :> [`Ok | error]);
    t.error_handler error;
  ;;

  let report_exn t exn =
    set_error_and_handle t (`Exn exn)
  ;;

  let flush_response_body t =
    match !(t.state) with
    | Awaiting_response | Closed -> ()
    | Received_response(_, response_body) ->
      try Body.execute_read response_body
      with exn -> report_exn t exn
  ;;

  let _next_read_operation t =
    match !(t.state) with
    | Awaiting_response | Closed -> Reader.next t.reader
    | Received_response(_, response_body) ->
      if not (Body.is_closed response_body)
      then Reader.next t.reader
      else begin
        Reader.close t.reader;
        Reader.next  t.reader
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

  let read t bs ~off ~len =
    let consumed = Reader.read t.reader bs ~off ~len in
    flush_response_body t;
    consumed
  ;;

  let next_write_operation t =
    flush_request_body t;
    Writer.next t.writer
  ;;

  let yield_writer t k =
    if Body.is_closed t.request_body
    then begin
      Writer.close t.writer;
      k ()
    end else
      Body.when_ready_to_write t.request_body k

  let report_write_result t result =
    Writer.report_result t.writer result

  let is_closed t = Reader.is_closed t.reader && Writer.is_closed t.writer
end
