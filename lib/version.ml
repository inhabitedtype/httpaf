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


type t =
  { major : int
  ; minor : int }

let v1_0 = { major = 1; minor = 0 }
let v1_1 = { major = 1; minor = 1 }

let to_buffer b t =
  Buffer.add_string b "HTTP/";
  Buffer.add_string b (string_of_int t.major);
  Buffer.add_char   b '.';
  Buffer.add_string b (string_of_int t.minor)

let compare x y =
  let c = compare x.major y.major in
  if c <> 0 then c else compare x.minor y.minor

let to_string t =
  match t with
  | { major = 1; minor = 0 } -> "HTTP/1.0"
  | { major = 1; minor = 1 } -> "HTTP/1.1"
  | _ ->
    let b = Buffer.create 8 in
    to_buffer b t;
    Buffer.contents b

let of_string = function
  | "HTTP/1.1" -> { major = 1; minor = 1 }
  | "HTTP/1.0" -> { major = 1; minor = 0 }
  | s ->
    try Scanf.sscanf s "HTTP/%d.%d" (fun major minor -> { major; minor })
    with _ -> raise (Failure "Version.of_string")

let pp_hum fmt t =
  Format.fprintf fmt "HTTP/%d.%d" t.major t.minor
