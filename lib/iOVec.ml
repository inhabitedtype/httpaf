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


type 'a t = 'a Faraday.iovec =
  { buffer : 'a
  ; off : int
  ; len : int }

let length { len; _ } = len
let lengthv iovs = List.fold_left (fun acc { len; _ } -> acc + len) 0 iovs

let shift { buffer; off; len } n =
  assert (n <= len);
  { buffer; off = off + n; len = len - n }

let shiftv iovecs n =
  if n < 0 then failwith (Printf.sprintf "IOVec.shiftv: %d is a negative number" n);
  let rec loop iovecs n =
    if n = 0
    then iovecs
    else match iovecs with
    | []            -> failwith "shiftv: n > lengthv iovecs"
    | iovec::iovecs ->
      let iovec_len = length iovec in
      if iovec_len <= n
      then loop iovecs (n - iovec_len)
      else (shift iovec n)::iovecs
  in
  loop iovecs n

let add_len { buffer; off; len } n =
  { buffer; off; len = len + n }

let pp_hum fmt t =
  Format.fprintf fmt "{ buffer = <opaque>; off = %d; len = %d }" t.off t.len
