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


include Angstrom

module P = struct
  let is_space =
    function | ' ' | '\t' -> true | _ -> false

  let is_cr =
    function | '\r' -> true | _ -> false

  let is_space_or_colon =
    function | ' ' | '\t' | ':' -> true | _ -> false

  let is_hex =
    function | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

  let is_digit =
    function '0' .. '9' -> true | _ -> false

  let is_separator =
    function
      | ')' | '(' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"'
      | '/' | '[' | ']' | '?' | '=' | '{' | '}' | ' ' | '\t' -> true
      | _ -> false

  let is_token =
    (* The commented-out ' ' and '\t' are not necessary because of the range at
     * the top of the match. *)
    function
      | '\000' .. '\031' | '\127'
      | ')' | '(' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"'
      | '/' | '[' | ']' | '?' | '=' | '{' | '}' (* | ' ' | '\t' *) -> false
      | _ -> true
end

let unit = return ()
let token = take_while1 P.is_token
let spaces = skip_while P.is_space

let digit =
  satisfy P.is_digit
  >>| function
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5
    | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9 | _ -> assert false

let eol = string "\r\n" <?> "eol"
let hex str =
  try return (Int64.of_string ("0x" ^ str)) with _ -> fail "hex"
let skip_line = take_till P.is_cr *> eol

let version =
  string "HTTP/" *>
  lift2 (fun major minor -> { Version.major; minor })
    (digit <* char '.')
    digit

let header =
  (* From RFC7230§3.2.4:

       "No whitespace is allowed between the header field-name and colon.  In
       the past, differences in the handling of such whitespace have led to
       security vulnerabilities in request routing and response handling.  A
       server MUST reject any received request message that contains whitespace
       between a header field-name and colon with a response code of 400 (Bad
       Request).  A proxy MUST remove any such whitespace from a response
       message before forwarding the message downstream."

    This can be detected by checking the message and marks in a parse failure,
    which should look like this when serialized "... > header > :". *)
  lift2 (fun key value -> (key, value))
    (take_till P.is_space_or_colon <* char ':' <* spaces)
    (take_till P.is_cr <* eol >>| String.trim)
  <?> "header"

let headers =
  let cons x xs = x :: xs in
  fix (fun headers ->
    let _emp = return [] in
    let _rec = lift2 cons header headers in
    peek_char_fail
    >>= function
      | '\r' -> _emp
      | _    -> _rec)

let request =
  let meth = take_till P.is_space >>| Method.of_string in
  lift4 (fun meth target version headers ->
    Request.create ~version ~headers meth target)
    (meth                 <* char ' ')
    (take_till P.is_space <* char ' ')
    (version              <* eol <* commit)
    (headers              <* eol)

let response =
  let status = take_till P.is_space >>| Status.of_string in
  lift4 (fun version status reason headers ->
    Response.create ~reason ~version ~headers status)
    (version              <* char ' ')
    (status               <* char ' ')
    (take_till P.is_cr    <* eol <* commit)
    (headers              <* eol)

let swallow_trailer =
  skip_many header *> eol *> commit

let finish writer =
  Request.Body.close writer;
  commit

let schedule_size writer n =
  let faraday = Request.Body.unsafe_faraday writer in
  (* XXX(seliopou): performance regression due to switching to a single output
   * format in Farady. Once a specialized operation is exposed to avoid the
   * intemediate copy, this should be back to the original performance. *)
  begin if Faraday.is_closed faraday
  then advance n
  else take n >>| fun s -> Faraday.write_string faraday s
  end *> commit

let body ~encoding writer =
  let rec fixed n ~unexpected =
    if n = 0L
    then unit
    else
      at_end_of_input
      >>= function
        | true  ->
          finish writer *> fail unexpected
        | false ->
          available >>= fun m ->
          let m' = Int64.(min (of_int m) n) in
          let n' = Int64.sub n m' in
          schedule_size writer (Int64.to_int m') >>= fun () -> fixed n' ~unexpected
  in
  match encoding with
  | `Fixed n ->
    fixed n ~unexpected:"expected more from fixed body"
  | `Chunked ->
    fix (fun p ->
      let _hex =
        (take_while1 P.is_hex >>= fun size -> hex size)
        (* swallows chunk-ext, if present, and CRLF *)
        <* (skip_line *> commit)
      in
      at_end_of_input
      >>= function
        | true  -> finish writer
        | false ->
          _hex >>= fun size ->
          if size = 0L
          then finish writer *> swallow_trailer
          else fixed size ~unexpected:"expected more from body chunk" *> p)
  | `Close_delimited ->
    fix (fun p ->
      let _rec = (available >>= fun n -> schedule_size writer n) *> p in
      at_end_of_input
      >>= function
        | true  -> finish writer
        | false -> _rec)

module Reader = struct
  module AU = Angstrom.Unbuffered

  type error = [
    | `Bad_request of Request.t
    | `Parse of string list * string ]

  type t =
    { handler             : Request.t -> Request.Body.t -> unit
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
    let ok = return (Ok ()) in
    request <* commit >>= fun request ->
    match Request.body_length request with
    | `Error `Bad_request -> return (Error (`Bad_request request))
    | `Fixed 0L  ->
      handler request Request.Body.empty;
      ok
    | `Fixed _ | `Chunked | `Close_delimited as encoding ->
      let request_body = Request.Body.create Bigstring.empty in
      handler request request_body;
      body ~encoding request_body *> ok

  let create ?(buffer_size=0x1000) handler =
    let buffer = Bigstring.create buffer_size in
    { handler
    ; buffer
    ; off         = 0
    ; len         = 0
    ; parse_state = AU.parse (parser handler)
    ; closed      = false
    }

  let invariant t =
    assert
      (match t.parse_state with
      | AU.Done(committed, _) | AU.Partial { AU.committed; _ } -> committed = 0
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
    let { off; len; _ } = t in
    assert (n <= len);
    t.len <- len - n;
    t.off <- if len = n then 0 else off + n

  let buffer_for_parsing { buffer; off; len; _ } =
    Bigstring.sub ~off ~len buffer

  let buffer_for_read_operation t =
    let { buffer; off; len; _ } = t in
    if len = Bigstring.length buffer
    then `Error (`Parse([], "parser stall: input too large"))
    else `Read  (Bigstring.sub ~off:(off + len) buffer)

  let rec update_parse_state t more =
    (* Invariant: the [parse_state] has no bytes to commit after this fuction
     * has been called. *)
    match t.parse_state with
    | AU.Done(_, Error _) | AU.Fail _  -> ()
    | AU.Done(committed, (Ok () as result)) ->
      commit t committed;
      begin match more with
      | AU.Incomplete ->
        t.parse_state <- AU.parse (parser t.handler) ~input:(buffer_for_parsing t);
        update_parse_state t more
      | AU.Complete ->
        t.parse_state <- AU.Done(0, result)
      end
    | AU.Partial { AU.continue; _ } ->
      begin match continue (buffer_for_parsing t) more with
      | AU.Partial { AU.continue; committed } ->
        commit t committed;
        t.parse_state <- AU.Partial { AU.continue; committed = 0 };
      | parse_state ->
        t.parse_state <- parse_state;
        update_parse_state t more
      end

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
    | AU.Done(committed, Ok ()) ->
      assert (committed = 0); (* enforce the invariant of [update_parse_state] *)
      if t.closed
      then `Close
      else buffer_for_read_operation t
    | AU.Done(_, Error err)       -> `Error err
    | AU.Fail(0, _, _)            -> assert t.closed; `Close
    | AU.Fail(_, marks , message) -> `Error (`Parse(marks, message))
    | AU.Partial { AU.committed; _ } ->
      assert (committed = 0); (* enforce the invariant of [update_parse_state] *)
      if t.closed
      then (update_parse_state t AU.Complete; next t)
      else buffer_for_read_operation t
end
