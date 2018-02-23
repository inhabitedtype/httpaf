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

type error =
  [ `Bad_request | `Bad_gateway | `Internal_server_error | `Exn of exn ]

type 'handle response_state =
  | Waiting   of (unit -> unit) ref
  | Complete  of Response.t
  | Streaming of Response.t * [`write] Body.t

type error_handler =
  ?request:Request.t -> error -> (Headers.t -> [`write] Body.t) -> unit

module Writer = Serialize.Writer

(* XXX(seliopou): The current design assumes that a new [Reqd.t] will be
 * allocated for each new request/response on a connection. This is wasteful,
 * as it creates garbage on persistent connections. A better approach would be
 * to allocate a single [Reqd.t] per connection and reuse it across
 * request/responses. This would allow a single [Faraday.t] to be allocated for
 * the body and reused. The [response_state] type could then be inlined into
 * the [Reqd.t] record, with dummy values occuping the fields for [response].
 * Something like this:
 *
 * {[
 *   type 'handle t =
 *     { mutable request        : Request.t
 *     ; mutable request_body   : Response.Body.t
 *     ; mutable response       : Response.t (* Starts off as a dummy value,
 *                                            * using [(==)] to identify it when
 *                                            * necessary *)
 *     ; mutable response_body  : Response.Body.t
 *     ; mutable persistent     : bool
 *     ; mutable response_state : [ `Waiting | `Started | `Streaming ]
 *     }
 *  ]}
 *
 * *)
type 'handle t =
  { request                 : Request.t
  ; request_body            : [`read] Body.t
  ; writer                  : Writer.t
  ; response_body_buffer    : Bigstring.t
  ; error_handler           : error_handler
  ; mutable persistent      : bool
  ; mutable response_state  : 'handle response_state
  ; mutable error_code      : [`Ok | error ]
  ; wait_for_first_flush    : bool
  }

let default_waiting = Sys.opaque_identity (fun () -> ())

let create error_handler request request_body writer response_body_buffer =
  { request
  ; request_body
  ; writer
  ; response_body_buffer
  ; error_handler
  ; persistent              = Request.persistent_connection request
  ; response_state          = Waiting (ref default_waiting)
  ; error_code              = `Ok
    (* XXX(seliopou): Make it configurable whether this callback is fired upon
     * receiving the response, or after the first flush of the streaming body.
     * There's a tradeoff here between time to first byte (latency) and batching
     * (throughput). For now, just wait for the first flush. *)
  ; wait_for_first_flush    = true
  }

let done_waiting when_done_waiting =
  let f = !when_done_waiting in
  when_done_waiting := default_waiting;
  f ()

let request { request; _ } = request
let request_body { request_body; _ } = request_body

let response { response_state; _ } =
  match response_state with
  | Waiting _ -> None
  | Streaming(response, _)
  | Complete (response) -> Some response

let response_exn { response_state; _ } =
  match response_state with
  | Waiting _            -> failwith "httpaf.Reqd.response_exn: response has not started"
  | Streaming(response, _)
  | Complete (response) -> response

let respond_with_string t response str =
  if t.error_code <> `Ok then
    failwith "httpaf.Reqd.respond_with_string: invalid state, currently handling error";
  match t.response_state with
  | Waiting when_done_waiting ->
    (* XXX(seliopou): check response body length *)
    Writer.write_response  t.writer response;
    Writer.write_string t.writer str;
    if t.persistent then
      t.persistent <- Response.persistent_connection response;
    t.response_state <- Complete response;
    done_waiting when_done_waiting
  | Streaming _ ->
    failwith "httpaf.Reqd.respond_with_string: response already started"
  | Complete _ ->
    failwith "httpaf.Reqd.respond_with_string: response already complete"

let respond_with_bigstring t response (bstr:Bigstring.t) =
  if t.error_code <> `Ok then
    failwith "httpaf.Reqd.respond_with_bigstring: invalid state, currently handling error";
  match t.response_state with
  | Waiting when_done_waiting ->
    (* XXX(seliopou): check response body length *)
    Writer.write_response     t.writer response;
    Writer.schedule_bigstring t.writer bstr;
    if t.persistent then
      t.persistent <- Response.persistent_connection response;
    t.response_state <- Complete response;
    done_waiting when_done_waiting
  | Streaming _ ->
    failwith "httpaf.Reqd.respond_with_bigstring: response already started"
  | Complete _ ->
    failwith "httpaf.Reqd.respond_with_bigstring: response already complete"

let unsafe_respond_with_streaming t response =
  match t.response_state with
  | Waiting when_done_waiting ->
    let response_body = Body.create t.response_body_buffer in
    Writer.write_response t.writer response;
    if t.wait_for_first_flush then Writer.yield t.writer;
    if t.persistent then
      t.persistent <- Response.persistent_connection response;
    t.response_state <- Streaming(response, response_body);
    done_waiting when_done_waiting;
    response_body
  | Streaming _ ->
    failwith "httpaf.Reqd.respond_with_streaming: response already started"
  | Complete _ ->
    failwith "httpaf.Reqd.respond_with_streaming: response already complete"

let respond_with_streaming t response =
  if t.error_code <> `Ok then
    failwith "httpaf.Reqd.respond_with_streaming: invalid state, currently handling error";
  unsafe_respond_with_streaming t response

let report_error t error =
  t.persistent <- false;
  Body.close_reader t.request_body;
  match t.response_state, t.error_code with
  | Waiting _, `Ok ->
    t.error_code <- (error :> [`Ok | error]);
    let status =
      match (error :> [error | Status.standard]) with
      | `Exn _                     -> `Internal_server_error
      | #Status.standard as status -> status
    in
    t.error_handler ~request:t.request error (fun headers ->
      unsafe_respond_with_streaming t (Response.create ~headers status))
  | Waiting _, `Exn _ ->
    (* XXX(seliopou): Decide what to do in this unlikely case. There is an
     * outstanding call to the [error_handler], but an intervening exception
     * has been reported as well. *)
    failwith "httpaf.Reqd.report_exn: NYI"
  | Streaming(_response, response_body), `Ok ->
    Body.close_writer response_body
  | Streaming(_response, response_body), `Exn _ ->
    Body.close_writer response_body;
    Writer.close t.writer
  | (Complete _ | Streaming _ | Waiting _) , _ ->
    (* XXX(seliopou): Once additional logging support is added, log the error
     * in case it is not spurious. *)
    ()

let report_exn t exn =
  report_error t (`Exn exn)

let try_with t f : (unit, exn) Result.result =
  try f (); Ok () with exn -> report_exn t exn; Error exn

(* Private API, not exposed to the user through httpaf.mli *)

let close_request_body { request_body; _ } =
  Body.close_reader request_body

let error_code t =
  match t.error_code with
  | #error as error -> Some error
  | `Ok             -> None

let on_more_output_available t f =
  match t.response_state with
  | Waiting when_done_waiting ->
    if not (!when_done_waiting == default_waiting) then
      failwith "httpaf.Reqd.on_more_output_available: only one callback can be registered at a time";
    when_done_waiting := f
  | Streaming(_, response_body) ->
    Body.when_ready_to_write response_body f
  | Complete _ ->
    failwith "httpaf.Reqd.on_more_output_available: response already complete"

let persistent_connection t =
  t.persistent

let requires_input { request_body; _ } =
  not (Body.is_closed request_body)

let requires_output { response_state; _ } =
  match response_state with
  | Complete _ -> false
  | Streaming (_, response_body) ->
    not (Body.is_closed response_body)
    || Body.has_pending_output response_body
  | Waiting _ -> true

let is_complete t =
  not (requires_input t || requires_output t)

let flush_request_body t =
  let request_body = request_body t in
  if Body.has_pending_output request_body
  then try Body.execute_read request_body
  with exn -> report_exn t exn

let flush_response_body t =
  match t.response_state with
  | Streaming (response, response_body) ->
    let request_method = t.request.Request.meth in
    let encoding =
      match Response.body_length ~request_method response with
      | `Fixed _ | `Close_delimited | `Chunked as encoding -> encoding
      | `Error _ -> assert false (* XXX(seliopou): This needs to be handled properly *)
    in
    Body.transfer_to_writer_with_encoding response_body ~encoding t.writer
  | _ -> ()
