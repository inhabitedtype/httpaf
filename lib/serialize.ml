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

let write_response t { Response.version; status; reason; headers } =
  write_version t version; write_space t;
  write_status  t status ; write_space t;
  write_string  t reason ; write_crlf  t;
  write_headers t headers
