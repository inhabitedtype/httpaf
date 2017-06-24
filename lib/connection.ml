(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

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


open Result

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

module Rd = struct
  type response_state =
    | Waiting of (unit -> unit) list ref
    | Started of Response.t * Response.Body.t

  type t =
    { request                 : Request.t
    ; request_body            : Request.Body.t
    ; buffered_response_bytes : int ref
    ; mutable persistent      : bool
    ; mutable response_state  : response_state
    }

  let empty_handlers () = ref []

  let create request request_body =
    { request
    ; request_body
    ; buffered_response_bytes = ref 0
    ; persistent              = Request.persistent_connection request
    ; response_state          = Waiting (empty_handlers ())
    }

  let close_response_body t =
    match t.response_state with
    | Started(_, response_body) -> Response.Body.close response_body
    | Waiting _                 -> ()

  let response_started t =
    match t.response_state with
    | Waiting _ -> false
    | Started _ -> true

  let start_response t response response_body =
    match t.response_state with
    | Started _         -> assert false
    | Waiting callbacks ->
      if t.persistent then
        t.persistent <- Response.persistent_connection response;
      t.response_state <- Started(response, response_body);
      List.iter (fun f -> Response.Body.when_ready_to_write response_body f)
        !callbacks

  let persistent_connection t =
    t.persistent

  let requires_input { request_body } =
    not (Request.Body.is_closed request_body)

  let requires_output { response_state } =
    match response_state with
    | Waiting _                 -> true
    | Started(_, response_body) ->
      Response.Body.(has_pending_output response_body || not (is_closed response_body))

  let is_complete t =
    not (requires_input t || requires_output t)

  let flush_request_body t =
    if Request.Body.has_pending_output t.request_body then
      Request.Body.execute_read t.request_body

  let flush_response_body t writer =
    match t.response_state with
    | Waiting _                        -> ()
    | Started(response, response_body) ->
      let faraday = Response.Body.unsafe_faraday response_body in
      begin match Faraday.operation faraday with
      | `Yield | `Close -> ()
      | `Writev iovecs ->
        let buffered = t.buffered_response_bytes in
        let iovecs   = IOVec.shiftv  iovecs !buffered in
        let lengthv  = IOVec.lengthv iovecs in
        buffered := !buffered + lengthv;
        let request_method = t.request.Request.meth in
        begin match Response.body_length ~request_method response with
        | `Fixed _ | `Close_delimited -> Writer.schedule_fixed writer iovecs
        | `Chunked -> Writer.schedule_chunk writer iovecs
        | `Error _ -> assert false
        end;
        Writer.flush writer (fun () ->
          Faraday.shift faraday lengthv;
          buffered := !buffered - lengthv)
      end

  let on_more_output_available t k =
    (* Also handles the case where the body is closed *)
    match t.response_state with
    | Waiting callbacks         -> callbacks := k::!callbacks
    | Started(_, response_body) -> Response.Body.when_ready_to_write response_body k

  let invariant t =
    let (=>) a b = b || not a in
    assert (is_complete t => not (requires_input t));
    assert (is_complete t => not (requires_output t));
  ;;

end

module Config = struct
  type t =
    { read_buffer_size          : int
    ; response_buffer_size      : int
    ; response_body_buffer_size : int }

  let default =
    { read_buffer_size          = 0x1000
    ; response_buffer_size      = 0x400
    ; response_body_buffer_size = 0x1000 }
end

type 'fd request_handler = 'fd Reqd.t -> unit

type error =
  [ `Bad_gateway | `Bad_request | `Internal_server_error | `Exn of exn]

type error_handler =
  ?request:Request.t -> error -> (Headers.t -> Response.Body.t) -> unit

type 'fd t =
  { reader                 : Reader.t
  ; writer                 : Writer.t
  ; response_body_buffer   : Bigstring.t
  ; request_handler        : 'fd request_handler
  ; error_handler          : error_handler
  ; request_queue          : 'fd Reqd.t Queue.t
    (* invariant: If [request_queue] is not empty, then the head of the queue
       has already had [request_handler] called on it. *)
  ; wakeup_writer  : (unit -> unit) list ref
  ; wakeup_reader  : (unit -> unit) list ref
  }

let is_shutdown t =
  t.reader.Reader.closed && t.writer.Writer.closed

let is_waiting t =
  not (is_shutdown t) && Queue.is_empty t.request_queue

let is_active t =
  not (Queue.is_empty t.request_queue)

let current_reqd_exn t =
  Queue.peek_exn t.request_queue

let on_wakeup_reader t k =
  if is_shutdown t
  then failwith "on_wakeup_reader on closed conn"
  else t.wakeup_reader := k::!(t.wakeup_reader)

let on_wakeup_writer t k =
  if is_shutdown t
  then failwith "on_wakeup_writer on closed conn"
  else t.wakeup_writer := k::!(t.wakeup_writer)

let _wakeup_writer callbacks =
  let fs = !callbacks in
  callbacks := [];
  List.iter (fun f -> f ()) fs

let wakeup_writer t =
  _wakeup_writer t.wakeup_writer

let wakeup_reader t =
  let fs = !(t.wakeup_reader) in
  t.wakeup_reader := [];
  List.iter (fun f -> f ()) fs

let default_error_handler ?request error handle =
  let message =
    match error with
    | `Exn exn -> Printexc.to_string exn
    | (#Status.client_error | #Status.server_error) as error -> Status.to_string error
  in
  let body = handle Headers.empty in
  Response.Body.write_string body message;
  Response.Body.close body

let create ?(config=Config.default) ?(error_handler=default_error_handler) ~request_handler =
  let
    { Config
    . read_buffer_size
    ; response_buffer_size
    ; response_body_buffer_size
    } = config
  in
  let writer = Writer.create ~buffer_size:response_buffer_size () in
  let request_queue = Queue.create () in
  let wakeup_writer = ref [] in
  let response_body_buffer = Bigstring.create response_body_buffer_size in
  let handler request request_body =
    let handle_now = Queue.is_empty request_queue in
    let reqd       =
      Reqd.create error_handler request request_body writer response_body_buffer in
    Queue.push reqd request_queue;
    if handle_now then begin
      request_handler reqd;
      _wakeup_writer wakeup_writer
    end
  in
  { reader          = Reader.create ~buffer_size:read_buffer_size handler
  ; writer
  ; response_body_buffer
  ; request_handler = request_handler
  ; error_handler   = error_handler
  ; request_queue
  ; wakeup_writer
  ; wakeup_reader   = ref []
  }

let state t =
  match t.reader.Reader.closed, t.writer.Writer.closed with
  | false, false -> `Running
  | true , true  -> `Closed
  | true , false -> `Closed_input
  | false, true  -> assert false

let shutdown_reader t =
  Reader.close t.reader;
  if is_active t
  then Reqd.close_request_body (current_reqd_exn t)
  else wakeup_reader t

let shutdown_writer t =
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
    let status =
      match (error :> [error | Status.standard]) with
      | `Exn _                     -> `Internal_server_error
      | #Status.standard as status -> status
    in
    shutdown_reader t;
    let writer = t.writer in
    t.error_handler ?request error (fun headers ->
      Writer.write_response writer (Response.create ~headers status);
      Response.Body.of_faraday (Writer.faraday writer));
  end

let report_exn t exn =
  set_error_and_handle t (`Exn exn)

let advance_request_queue_if_necessary t =
  if is_active t then begin
    let reqd = current_reqd_exn t in
    if Reqd.persistent_connection reqd
    then if Reqd.is_complete reqd then begin
      ignore (Queue.take t.request_queue);
      wakeup_reader t;
    end else begin
      ignore (Queue.take t.request_queue);
      Queue.iter Reqd.close_request_body t.request_queue;
      Queue.clear t.request_queue;
      Queue.push reqd t.request_queue;
      wakeup_writer t;
      if Reqd.is_complete reqd
      then shutdown t
      else if not (Reqd.requires_input reqd)
      then shutdown_reader t
    end
  end else if Reader.is_closed t.reader
  then shutdown t

let _next_read_operation t =
  advance_request_queue_if_necessary t;
  if is_active t then begin
    let reqd = current_reqd_exn t in
    if      Reqd.requires_input        reqd then Reader.next t.reader
    else if Reqd.persistent_connection reqd then `Yield
    else begin
      shutdown_reader t;
      Reader.next t.reader
    end
  end else
    Reader.next t.reader

let next_read_operation t =
  match _next_read_operation t with
  | `Error (`Parse _)             -> set_error_and_handle          t `Bad_request; `Close
  | `Error (`Bad_request request) -> set_error_and_handle ~request t `Bad_request; `Close
  | (`Read _ | `Yield | `Close) as operation -> operation

let report_read_result t result =
  Reader.report_result t.reader result;
  if is_active t then
    Reqd.flush_request_body (current_reqd_exn t)

let yield_reader t k =
  on_wakeup_reader t k

let flush_response_body t =
  if is_active t then
    let reqd = current_reqd_exn t in
    Reqd.flush_response_body reqd

let rec next_write_operation t =
  advance_request_queue_if_necessary t;
  flush_response_body t;
  Writer.next t.writer

let report_write_result t result =
  Writer.report_result t.writer result

let yield_writer t k =
  if is_active t then begin
    let reqd = current_reqd_exn t in
    if Reqd.requires_output reqd
    then Reqd.on_more_output_available reqd k
    else if Reqd.persistent_connection reqd
    then on_wakeup_writer t k
    else begin shutdown t; k () end
  end else if Writer.is_closed t.writer then k () else begin
    on_wakeup_writer t k
  end
