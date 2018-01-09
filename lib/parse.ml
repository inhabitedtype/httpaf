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

let finish writer =
  Body.close_reader writer;
  commit

let schedule_size writer n =
  let faraday = Body.unsafe_faraday writer in
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
    >>= fun () -> finish writer
  | `Chunked ->
    (* XXX(seliopou): The [eol] in this parser should really parse a collection
     * of "chunk extensions", as defined in RFC7230§4.1. These do not show up
     * in the wild very frequently, and the httpaf API has no way of exposing
     * them to the suer, so for now the parser does not attempt to recognize
     * them. This means that any chunked messages that contain chunk extensions
     * will fail to parse. *)
    fix (fun p ->
      let _hex =
        (take_while1 P.is_hex >>= fun size -> hex size)
        (* swallows chunk-ext, if present, and CRLF *)
        <* (eol *> commit)
      in
      _hex >>= fun size ->
      if size = 0L
      then finish writer
      else fixed size ~unexpected:"expected more from body chunk" *> eol *> p)
  | `Close_delimited ->
    fix (fun p ->
      let _rec = (available >>= fun n -> schedule_size writer n) *> p in
      at_end_of_input
      >>= function
        | true  -> finish writer
        | false -> _rec)

module Reader = struct
  module AU = Angstrom.Unbuffered

  type request_error = [
    | `Bad_request of Request.t
    | `Parse of string list * string ]

  type response_error = [
    | `Invalid_response_body_length of Response.t
    | `Parse of string list * string ]

  type 'error parse_state =
    | Done
    | Fail    of 'error
    | Partial of (Bigstring.t -> off:int -> len:int -> AU.more -> (unit, 'error) result AU.state)

  type 'error t =
    { parser              : (unit, 'error) result Angstrom.t
    ; mutable parse_state : 'error parse_state
      (* The state of the parse for the current request *)
    ; mutable closed      : bool
      (* Whether the input source has left the building, indicating that no
       * further input will be received. *)
    }

  type request  = request_error t
  type response = response_error t

  let create parser =
    { parser
    ; parse_state = Done
    ; closed      = false
    }

  let ok = return (Ok ())

  let request handler =
    let parser =
      request <* commit >>= fun request ->
      match Request.body_length request with
      | `Error `Bad_request -> return (Error (`Bad_request request))
      | `Fixed 0L  ->
        handler request Body.empty;
        ok
      | `Fixed _ | `Chunked | `Close_delimited as encoding ->
        let request_body = Body.create Bigstring.empty in
        handler request request_body;
        body ~encoding request_body *> ok
    in
    create parser

  let response ~request_method handler =
    let parser =
      response <* commit >>= fun response ->
      let proxy = false in
      match Response.body_length ~request_method response with
      | `Error `Bad_gateway           -> assert (not proxy); assert false
      | `Error `Internal_server_error -> return (Error (`Invalid_response_body_length response))
      | `Fixed 0L ->
        handler response Body.empty;
        ok
      | `Fixed _ | `Chunked | `Close_delimited as encoding ->
        let response_body = Body.create Bigstring.empty in
        handler response response_body;
        body ~encoding response_body *> ok
    in
    create parser
  ;;

  let close t =
    t.closed <- true

  let is_closed t =
    t.closed

  let transition t state =
    match state with
    | AU.Done(consumed, Ok ())
    | AU.Fail(0 as consumed, _, _) ->
      t.parse_state <- Done;
      consumed
    | AU.Done(consumed, Error error) ->
      t.parse_state <- Fail error;
      consumed
    | AU.Fail(consumed, marks, msg) ->
      t.parse_state <- Fail (`Parse(marks, msg));
      consumed
    | AU.Partial { committed; continue } ->
      t.parse_state <- Partial continue;
      committed
  and start t state =
      match state with
      | AU.Done _         -> failwith "httpaf.Parse.unable to start parser"
      | AU.Fail(0, marks, msg) ->
        t.parse_state <- Fail (`Parse(marks, msg))
      | AU.Partial { committed = 0; continue } ->
        t.parse_state <- Partial continue
      | _ -> assert false
  ;;

  let rec read t bs ~off ~len =
    match t.parse_state with
    | Fail _ -> 0
    | Done   ->
      start t (AU.parse t.parser);
      read  t bs ~off ~len;
    | Partial continue ->
      transition t (continue bs Incomplete ~off ~len)
  ;;

  let next t =
    match t.parse_state with
    | Done ->
      if t.closed
      then `Close
      else `Read
    | Fail    _ -> `Close
    | Partial _ -> `Read
end
