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
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let create len =
  Bigarray.(Array1.create char c_layout len)

module BA1 = Bigarray.Array1

let length t = BA1.dim t

let get t i = BA1.get t i
let unsafe_get t i = BA1.unsafe_get t i

let set t i = BA1.set t i
let unsafe_set t i = BA1.unsafe_set t i

let blit src src_off dst dst_off len =
  BA1.(blit (sub src src_off len) (sub dst dst_off len))

let blit_from_string src src_off dst dst_off len =
  for i = 0 to len - 1 do
    BA1.unsafe_set dst (dst_off + i) (String.unsafe_get src (src_off + i))
  done

let blit_from_bytes src src_off dst dst_off len =
  blit_from_string (Bytes.unsafe_to_string src) src_off dst dst_off len

let blit_to_bytes src src_off dst dst_off len =
  for i = 0 to len - 1 do
    Bytes.unsafe_set dst (dst_off + i) (BA1.unsafe_get src (src_off + i))
  done

let sub ~off ?len t =
  let len =
    match len with
    | None -> length t - off
    | Some len -> len
  in
  BA1.sub t off len

let to_string ?(off=0) ?len t =
  let len =
    match len with
    | None     -> length t - off
    | Some len -> len
  in
  let b = Bytes.create len in
  blit_to_bytes t off b 0 len;
  Bytes.unsafe_to_string b

let of_string ?(off=0) ?len s =
  let len =
    match len with
    | None     -> String.length s
    | Some len -> len
  in
  let b = create len in
  blit_from_string s off b 0 len;
  b

let empty = create 0
