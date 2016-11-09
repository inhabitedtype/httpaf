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

module Reader = struct
  module AU = Angstrom.Unbuffered

  type error = [
    | `Bad_request of Request.t
    | `Parse of string list * string ]

  type t =
    { handler             : Request.t -> Body.R.t -> unit
      (* The application request handler. *)
    ; buffer              : Bigstring.t
      (* The buffer that the parser reads from. Managed by the control module
       * for the reader. *)
    ; mutable off         : int
      (* The start of the readable region of {buffer}. *)
    ; mutable len         : int
      (* The length of the readable region of {buffer}. *)
    ; mutable parse_state : (unit, error) result AU.state
      (* The state of the parse for the current request *)
    ; mutable closed      : bool
      (* Whether the input source has left the building, indicating that no
       * further input will be received. *)
    }

  let parser handler =
    let open Parse in
    let ok = return (Ok ()) in
    request >>= fun request ->
    match Request.body_length request with
    | `Error `Bad_request -> return (Error (`Bad_request request))
    | `Fixed 0L  ->
      handler request Body.R.empty;
      ok
    | `Fixed _ | `Chunked | `Close_delimited as encoding ->
      let request_body as writer = Body.create ~buffer_size:0 () in
      handler request request_body;
      body ~encoding writer *> ok

  let create ?(buffer_size=0x1000) handler =
    let buffer = Bigstring.create buffer_size in
    { handler
    ; buffer
    ; off         = 0
    ; len         = 0
    ; parse_state = AU.parse Angstrom.(parser handler)
    ; closed      = false
    }

  let invariant t =
    assert
      (match t.parse_state with
      | AU.Done(committed, _) | AU.Partial { AU.committed } -> committed = 0
      | AU.Fail _ -> true);
    assert (t.len <= Bigstring.length t.buffer);
    assert (t.off <  Bigstring.length t.buffer);
    assert (t.off >= 0);
  ;;

  let close t =
    t.closed <- true

  let is_closed t =
    t.closed

  let commit t n =
    let { off; len } = t in
    t.len <- len - n;
    t.off <- if len = n then 0 else off + n

  let buffer_for_reading { buffer; off; len } =
    `Bigstring (Bigstring.sub ~off ~len buffer)

  let update_parse_state t more =
    match t.parse_state with
    | AU.Done(_, Error _) | AU.Fail _ -> ()
    | AU.Done(committed, Ok ())       ->
      commit t committed;
      if more = AU.Incomplete
      then t.parse_state <- AU.parse (parser t.handler);
    | AU.Partial { AU.continue; committed } ->
      commit t committed;
      t.parse_state <- continue (buffer_for_reading t) more

  let report_result t result =
    match result with
    | `Ok 0   -> ()
    | `Ok len ->
      let len = t.len + len in
      if len + t.off > Bigstring.length t.buffer then
        failwith "Reader.report_result size of read exceeds size of buffer";
      t.len <- len;
      update_parse_state t AU.Incomplete
    | `Eof ->
      update_parse_state t AU.Complete;
      close t

  let rec next t =
    match t.parse_state with
    | AU.Done(_, Ok ()) ->
      if t.closed then `Close
      else begin update_parse_state t AU.Incomplete; next t end
    | AU.Fail(0, _, _) ->
      assert t.closed;
      `Close
    | AU.Done(_, Error err) ->
      `Error err
    | AU.Fail(_, marks , message)  ->
      `Error (`Parse(marks, message))
    | AU.Partial { AU.committed } ->
      assert (committed = 0);
      if t.closed then begin update_parse_state t AU.Complete; next t end
      else
        let { buffer; off; len } = t in
        if len = Bigstring.length buffer
        then `Error (`Parse([], "parser stall: input too large"))
        else `Read(Bigstring.sub ~off:(off + len) buffer)
end

module Writer = struct
  module F = Faraday

  type t =
    { buffer                : Bigstring.t
      (* The buffer that the encoder uses for buffered writes. Managed by the
       * control module for the encoder. *)
    ; encoder               : F.t
      (* The encoder that handles encoding for writes. Uses the [buffer]
       * referenced above internally. *)
    ; mutable closed        : bool
      (* Whether the output source has left the building, indicating that no
       * further output should be generated. *)
    ; mutable drained_bytes : int
      (* The number of bytes that were not written due to the output stream
       * being closed before all buffered output could be written. Useful for
       * detecting error cases. *)
    }

  let create ?(buffer_size=0x800) () =
    let buffer = Bigstring.create buffer_size in
    let encoder = F.of_bigstring buffer in
    { buffer
    ; encoder
    ; closed        = false
    ; drained_bytes = 0
    }

  let invariant t =
    let (=>) a b = b || (not a) in
    let (<=>) a b = (a => b) && (b => a) in
    let writev, close, yield =
      match F.operation t.encoder with
      | `Writev _ -> true, false, false
      | `Close    -> false, true, false
      | `Yield    -> F.yield t.encoder; false, false, true
    in
    assert (t.closed <=> F.is_closed t.encoder);
    assert (F.is_closed t.encoder <=> close);
    assert (t.drained_bytes > 0 => t.closed);
  ;;

  let write_response t response =
    Serialize.write_response t.encoder response

  let schedule_fixed t iovecs =
    let s2b = Bytes.unsafe_of_string in
    List.iter (fun { IOVec.buffer; off; len } ->
      match buffer with
      | `String str   -> F.schedule_bytes     t.encoder ~off ~len (s2b str)
      | `Bytes bytes  -> F.schedule_bytes     t.encoder ~off ~len bytes
      | `Bigstring bs -> F.schedule_bigstring t.encoder ~off ~len bs)
    iovecs

  let schedule_chunk t iovecs =
    let len = Int64.of_int (IOVec.lengthv iovecs) in
    F.write_string t.encoder (Printf.sprintf "%Lx\r\n" len);
    schedule_fixed t iovecs

  let flush t f =
    F.flush t.encoder f

  let close t =
    t.closed <- true;
    F.close t.encoder;
    let drained = F.drain t.encoder in
    t.drained_bytes <- t.drained_bytes + drained

  let drained_bytes t =
    t.drained_bytes

  let report_result t result =
    match result with
    | `Closed -> close t
    | `Ok len -> F.shift t.encoder len

  let next t =
    match F.operation t.encoder with
    | `Close -> `Close (drained_bytes t)
    | `Yield -> `Yield
    | `Writev iovecs ->
      assert (not (t.closed));
      `Write ((iovecs:IOVec.buffer IOVec.t list))
end

module Rd = struct
  type response_state =
    | Waiting of (unit -> unit) list ref
    | Started of Response.t * Body.W.t

  type t =
    { request                 : Request.t
    ; request_body            : Body.R.t
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

  let close_request_body t =
    Body.close t.request_body

  let close_response_body t =
    match t.response_state with
    | Started(_, response_body) -> Body.close response_body
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
      List.iter (fun f -> Body.on_more_output_available response_body f)
        !callbacks

  let persistent_connection t =
    t.persistent

  let requires_input { request_body } =
    not (Body.is_closed request_body)

  let requires_output { response_state } =
    match response_state with
    | Waiting _                 -> true
    | Started(_, response_body) ->
      Body.(has_pending_output response_body || not (is_closed response_body))

  let is_complete t =
    not (requires_input t || requires_output t)

  let flush_request_body t =
    if Body.has_pending_output t.request_body then
      Body.execute_read t.request_body

  let flush_response_body t writer =
    match t.response_state with
    | Waiting _                        -> ()
    | Started(response, response_body) ->
      let faraday = response_body.Body.faraday in
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
    | Started(_, response_body) -> Body.on_more_output_available response_body k

  let invariant t =
    let (=>) a b = b || not a in
    assert (is_complete t => not (requires_input t));
    assert (is_complete t => not (requires_output t));
  ;;

end
