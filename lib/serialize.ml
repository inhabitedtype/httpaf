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

open Faraday

let write_space t   = write_char t ' '
let write_crlf  t   = write_string t "\r\n"

let write_version t version =
  write_string t (Version.to_string version)

let write_method t meth =
  write_string t (Method.to_string meth)

let write_status t status =
  write_string t (Status.to_string status)

let write_headers t headers =
  (* XXX(seliopou): escape these thigns *)
  List.iter (fun (name, value) ->
    write_string t name;
    write_string t ": ";
    write_string t value;
    write_crlf   t)
  (Headers.to_list headers);
  write_crlf t

let write_request t { Request.meth; target; version; headers } =
  write_method  t meth   ; write_space t;
  write_string  t target ; write_space t;
  write_version t version; write_crlf  t;
  write_headers t headers

let write_response t { Response0.version; status; reason; headers } =
  write_version t version; write_space t;
  write_status  t status ; write_space t;
  write_string  t reason ; write_crlf  t;
  write_headers t headers

let write_chunk_length t len =
  write_string t (Printf.sprintf "%x" len);
  write_crlf   t

let write_string_chunk t chunk =
  write_chunk_length t (String.length chunk);
  write_string       t chunk

let write_bigstring_chunk t chunk =
  write_chunk_length t (Bigstring.length chunk);
  write_bigstring    t chunk

let schedule_bigstring_chunk t chunk =
  write_chunk_length t (Bigstring.length chunk);
  schedule_bigstring t chunk

module Writer = struct
  type t =
    { buffer                : Bigstring.t
      (* The buffer that the encoder uses for buffered writes. Managed by the
       * control module for the encoder. *)
    ; encoder               : Faraday.t
      (* The encoder that handles encoding for writes. Uses the [buffer]
       * referenced above internally. *)
    ; mutable drained_bytes : int
      (* The number of bytes that were not written due to the output stream
       * being closed before all buffered output could be written. Useful for
       * detecting error cases. *)
    }

  let create ?(buffer_size=0x800) () =
    let buffer = Bigstring.create buffer_size in
    let encoder = Faraday.of_bigstring buffer in
    { buffer
    ; encoder
    ; drained_bytes = 0
    }

  let faraday t = t.encoder

  let write_request t request =
    write_request t.encoder request

  let write_response t response =
    write_response t.encoder response

  let write_string t ?off ?len string =
    write_string t.encoder ?off ?len string

  let write_bytes t ?off ?len bytes =
    write_bytes t.encoder ?off ?len bytes

  let write_bigstring t ?off ?len bigstring =
    write_bigstring t.encoder ?off ?len bigstring

  let schedule_bigstring t ?off ?len bigstring =
    schedule_bigstring t.encoder ?off ?len bigstring

  let schedule_fixed t iovecs =
    List.iter (fun { IOVec.buffer; off; len } ->
      schedule_bigstring t ~off ~len buffer)
    iovecs

  let schedule_chunk t iovecs =
    let length = IOVec.lengthv iovecs in
    write_chunk_length t.encoder length;
    schedule_fixed t iovecs

  let flush t f =
    flush t.encoder f

  let yield t =
    Faraday.yield t.encoder

  let close t =
    close t.encoder;
    let drained = Faraday.drain t.encoder in
    t.drained_bytes <- t.drained_bytes + drained

  let is_closed t =
    Faraday.is_closed t.encoder

  let drained_bytes t =
    t.drained_bytes

  let report_result t result =
    match result with
    | `Closed -> close t
    | `Ok len -> shift t.encoder len

  let next t =
    match Faraday.operation t.encoder with
    | `Close         -> `Close (drained_bytes t)
    | `Yield         -> `Yield
    | `Writev iovecs -> `Write iovecs
end
