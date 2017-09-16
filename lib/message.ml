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


(* This module contains functionality that applies to both requests and
   responses, which are collectively referred to in the HTTP 1.1 specifications
   as 'messages'. *)

let persistent_connection ?(proxy=false) version headers =
  let _ = proxy in
  (* XXX(seliopou): use proxy argument in the case of HTTP/1.0 *)
  match Headers.get headers "connection" with
  | Some "close"      -> false
  | Some "keep-alive" -> Version.(compare version v1_0) >= 0
  | _                 -> Version.(compare version v1_1) >= 0

let sort_uniq xs =
  (* Though {!List.sort_uniq} performs a check on the input length and returns
   * immediately for lists of length less than [2], it still allocates closures
   * before it does that check! To avoid that just do our own checking here to
   * avoid the allocations in the common case. *)
  match xs with
  | [] | [ _ ] -> xs
  | _          -> List.sort_uniq String.compare xs

let unique_content_length_values headers =
  (* XXX(seliopou): perform proper content-length parsing *)
  sort_uniq (Headers.get_multi headers "content-length")

let content_length_of_string s =
  try Int64.of_string s with _ -> -1L
