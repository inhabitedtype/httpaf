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

module Config = struct
  type t =
    { response_buffer_size      : int
    ; response_body_buffer_size : int }

  let default =
    { response_buffer_size      = 0x400
    ; response_body_buffer_size = 0x1000 }
end

type 'fd request_handler = 'fd Reqd.t -> unit

type error =
  [ `Bad_gateway | `Bad_request | `Internal_server_error | `Exn of exn]

type error_handler =
  ?request:Request.t -> error -> (Headers.t -> [`write] Body.t) -> unit

type 'fd t =
  { reader                 : Reader.request
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
  Reader.is_closed t.reader && Writer.is_closed t.writer

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

let default_error_handler ?request:_ error handle =
  let message =
    match error with
    | `Exn exn -> Printexc.to_string exn
    | (#Status.client_error | #Status.server_error) as error -> Status.to_string error
  in
  let body = handle Headers.empty in
  Body.write_string body message;
  Body.close_writer body
;;

let create ?(config=Config.default) ?(error_handler=default_error_handler) request_handler =
  let
    { Config
    . response_buffer_size
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
  { reader          = Reader.request handler
  ; writer
  ; response_body_buffer
  ; request_handler = request_handler
  ; error_handler   = error_handler
  ; request_queue
  ; wakeup_writer
  ; wakeup_reader   = ref []
  }

let is_closed t = Reader.is_closed t.reader && Writer.is_closed t.writer

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
      Body.of_faraday (Writer.faraday writer));
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
  | (`Read | `Yield | `Close) as operation -> operation

let read t bs ~off ~len =
  let consumed = Reader.read t.reader bs ~off ~len in
  if is_active t then
    Reqd.flush_request_body (current_reqd_exn t);
  consumed
;;

let yield_reader t k =
  on_wakeup_reader t k
;;

let flush_response_body t =
  if is_active t then
    let reqd = current_reqd_exn t in
    Reqd.flush_response_body reqd
;;

let next_write_operation t =
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
