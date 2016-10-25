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
  Body.close writer;
  commit

let schedule_size writer n =
  begin if Body.is_closed writer
  then advance n
  else take n >>| fun s -> Body.schedule_string writer s
  end *> commit

let rec body ~encoding writer =
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
