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
