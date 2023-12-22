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


module Queue = struct
  include Queue

  let peek_exn = peek

  let peek t =
    if is_empty t
    then None
    else Some (peek_exn t)
end

module Reader = Parse.Reader
module Writer = Serialize.Writer


type request_handler = Reqd.t -> unit

type error =
  [ `Bad_gateway | `Bad_request | `Internal_server_error | `Exn of exn]

type error_handler =
  ?request:Request.t -> error -> (Headers.t -> Body.Writer.t) -> unit

type t =
  { reader                 : Reader.request
  ; writer                 : Writer.t
  ; response_body_buffer   : Bigstringaf.t
  ; request_handler        : request_handler
  ; error_handler          : error_handler
  ; request_queue          : Reqd.t Queue.t
    (* invariant: If [request_queue] is not empty, then the head of the queue
       has already had [request_handler] called on it. *)
  ; mutable is_errored     : bool
    (* if there is a parse or connection error, we invoke the [error_handler]
       and set [is_errored] to indicate we should not close the writer yet. *)
  ; mutable wakeup_reader  : Optional_thunk.t
  }

let is_closed t =
  Reader.is_closed t.reader && Writer.is_closed t.writer

let is_active t =
  not (Queue.is_empty t.request_queue)

let current_reqd_exn t =
  Queue.peek_exn t.request_queue

let yield_reader t k =
  if is_closed t
  then failwith "yield_reader on closed conn"
  else if Optional_thunk.is_some t.wakeup_reader
  then failwith "yield_reader: only one callback can be registered at a time"
  else t.wakeup_reader <- Optional_thunk.some k
;;

let wakeup_reader t =
  let f = t.wakeup_reader in
  t.wakeup_reader <- Optional_thunk.none;
  Optional_thunk.call_if_some f
;;

let yield_writer t k =
 if Writer.is_closed t.writer
 then k ()
 else Writer.on_wakeup t.writer k
;;

let wakeup_writer t = Writer.wakeup t.writer

let default_error_handler ?request:_ error handle =
  let message =
    match error with
    | `Exn exn -> Printexc.to_string exn
    | (#Status.client_error | #Status.server_error) as error -> Status.to_string error
  in
  let body = handle Headers.empty in
  Body.Writer.write_string body message;
  Body.Writer.close body
;;

let create ?(config=Config.default) ?(error_handler=default_error_handler) request_handler =
  let
    { Config
    . response_buffer_size
    ; response_body_buffer_size
    ; _ } = config
  in
  let writer = Writer.create ~buffer_size:response_buffer_size () in
  let request_queue = Queue.create () in
  let response_body_buffer = Bigstringaf.create response_body_buffer_size in
  let handler request request_body =
    let reqd =
      Reqd.create error_handler request request_body writer response_body_buffer
    in
    Queue.push reqd request_queue;
  in
  { reader          = Reader.request handler
  ; writer
  ; response_body_buffer
  ; request_handler = request_handler
  ; error_handler   = error_handler
  ; request_queue
  ; is_errored      = false
  ; wakeup_reader   = Optional_thunk.none
  }

let shutdown_reader t =
  Reader.force_close t.reader;
  if is_active t
  then Reqd.close_request_body (current_reqd_exn t)
  else wakeup_reader t

let shutdown_writer t =
  if is_active t then Reqd.flush_response_body (current_reqd_exn t);
  Writer.close t.writer;
  if is_active t
  then Reqd.close_request_body (current_reqd_exn t)
  else wakeup_writer t

let error_code t =
  if is_active t
  then Reqd.error_code (current_reqd_exn t)
  else None

let shutdown t =
  shutdown_reader t;
  shutdown_writer t;
  wakeup_reader t;
  wakeup_writer t

let set_error_and_handle ?request t error =
  if is_active t then begin
    assert (request = None);
    let reqd = current_reqd_exn t in
    Reqd.report_error reqd error
  end else begin
    t.is_errored <- true;
    let status =
      match (error :> [error | Status.standard]) with
      | `Exn _                     -> `Internal_server_error
      | #Status.standard as status -> status
    in
    shutdown_reader t;
    let writer = t.writer in
    t.error_handler ?request error (fun headers ->
      let response = Response.create ~headers status in
      Writer.write_response writer response;
      let encoding =
        (* If we haven't parsed the request method, just use GET as a standard
           placeholder. The method is only used for edge cases, like HEAD or
           CONNECT. *)
        let request_method =
          match request with
          | None -> `GET
          | Some request -> request.meth
        in
        match Response.body_length ~request_method response with
        | `Fixed _ | `Close_delimited as encoding -> encoding
        | `Chunked ->
          (* XXX(dpatti): Because we pass the writer's faraday directly to the
             new body, we don't write the chunked encoding. A client won't be
             able to interpret this. *)
          `Close_delimited
        | `Error (`Bad_gateway | `Internal_server_error) ->
          failwith "httpaf.Server_connection.error_handler: invalid response body length"
      in
      Body.Writer.of_faraday (Writer.faraday writer) ~encoding
        ~when_ready_to_write:(fun () -> Writer.wakeup writer));
  end

let report_exn t exn =
  set_error_and_handle t (`Exn exn)

let advance_request_queue t =
  ignore (Queue.take t.request_queue);
  if not (Queue.is_empty t.request_queue)
  then t.request_handler (Queue.peek_exn t.request_queue);
;;

let rec _next_read_operation t =
  if not (is_active t)
  then (
    (* If the request queue is empty, there is no connection error, and the
       reader is closed, then we can assume that no more user code will be able
       to write. *)
    if Reader.is_closed t.reader && not t.is_errored
    then shutdown_writer t;
    Reader.next t.reader
  ) else (
    let reqd = current_reqd_exn t in
    match Reqd.input_state reqd with
    | Waiting  -> `Yield
    | Ready    -> Reader.next t.reader
    | Complete -> _final_read_operation_for t reqd
    | Upgraded -> `Upgrade
  )

and _final_read_operation_for t reqd =
  if not (Reqd.persistent_connection reqd) then (
    shutdown_reader t;
    Reader.next t.reader;
  ) else (
    match Reqd.output_state reqd with
    | Waiting | Ready ->
      (* XXX(dpatti): This is a way in which the reader and writer are not
         parallel -- we tell the writer when it needs to yield but the reader is
         always asking for more data. This is the only branch in either
         operation function that does not return `(Reader|Writer).next`, which
         means there are surprising states you can get into. For example, we ask
         the runtime to yield but then raise when it tries to because the reader
         is closed. I don't think checking `is_closed` here makes sense
         semantically, but I don't think checking it in `_next_read_operation`
         makes sense either. I chose here so I could describe why. *)
      if Reader.is_closed t.reader
      then Reader.next t.reader
      else `Yield
    | Upgraded -> `Upgrade
    | Complete ->
      advance_request_queue t;
      _next_read_operation t;
  )
;;

let next_read_operation t =
  match _next_read_operation t with
  | `Error (`Parse _)             -> set_error_and_handle          t `Bad_request; `Close
  | `Error (`Bad_request request) -> set_error_and_handle ~request t `Bad_request; `Close
  | (`Read | `Yield | `Close | `Upgrade) as operation -> operation

let rec read_with_more t bs ~off ~len more =
  let call_handler = Queue.is_empty t.request_queue in
  let consumed = Reader.read_with_more t.reader bs ~off ~len more in
  if is_active t
  then (
    let reqd = current_reqd_exn t in
    if call_handler
    then t.request_handler reqd;
    Reqd.flush_request_body reqd;
  );
  (* Keep consuming input as long as progress is made and data is
     available, in case multiple requests were received at once. *)
  if consumed > 0 && consumed < len then
    let off = off + consumed
    and len = len - consumed in
    consumed + read_with_more t bs ~off ~len more
  else
    consumed
;;

let read t bs ~off ~len =
  read_with_more t bs ~off ~len Incomplete

let read_eof t bs ~off ~len =
  read_with_more t bs ~off ~len Complete

let rec _next_write_operation t =
  if not (is_active t)
  then Writer.next t.writer
  else (
    let reqd = current_reqd_exn t in
    match Reqd.output_state reqd with
    | Waiting -> Writer.next t.writer
    | Ready ->
      Reqd.flush_response_body reqd;
      Writer.next t.writer
    | Complete -> _final_write_operation_for t reqd ~upgrade:false
    | Upgraded -> _final_write_operation_for t reqd ~upgrade:true
  )

and _final_write_operation_for t reqd ~upgrade =
  let next =
    if upgrade then (
      if Writer.has_pending_output t.writer then
        (* Even in the Upgrade case, we're still responsible for writing the response
           header, so we might have work to do. *)
        Writer.next t.writer
      else
        `Upgrade
    ) else if not (Reqd.persistent_connection reqd) then (
      shutdown_writer t;
      Writer.next t.writer;
    ) else (
      match Reqd.input_state reqd with
      | Waiting -> `Yield
      | Ready -> Writer.next t.writer;
      | Upgraded -> `Upgrade
      | Complete ->
        advance_request_queue t;
        _next_write_operation t;
    )
  in
  (* The only reason the reader yields is to wait for the writer, so we need to
     notify it that we've completed. *)
  wakeup_reader t;
  next
;;

let next_write_operation t = _next_write_operation t

let report_write_result t result =
  Writer.report_result t.writer result
