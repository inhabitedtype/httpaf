(*----------------------------------------------------------------------------
    Copyright (c) 2018 Inhabited Type LLC.

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

(* XXX(dpatti): A [Body.t] is kind of a reader body and writer body stitched
   together into a single structure, but only half of it is used at any given
   time. The two uses are also quite different in that a writer body is always
   wired up to some [Writer.t] by httpaf internals at time of creation, whereas
   a reader body is given to the user as-is and the user is expected to drive
   the feeding of data into the body. It feels like they should simply be two
   separate types. *)

type 'a t =
  | Reader : Body_reader.t -> [ `read ] t
  | Writer : Body_writer.t -> [ `write ] t

let write_char (Writer body_writer : [ `write ] t) c =
  Body_writer.write_char body_writer c

let write_string (Writer body_writer : [ `write ] t) ?off ?len s =
  Body_writer.write_string body_writer ?off ?len s

let write_bigstring (Writer body_writer : [ `write ] t) ?off ?len b =
  Body_writer.write_bigstring body_writer ?off ?len b

let schedule_bigstring (Writer body_writer : [ `write ] t) ?off ?len b =
  Body_writer.schedule_bigstring body_writer ?off ?len b

let flush (Writer body_writer : [ `write ] t) kontinue =
  Body_writer.flush body_writer kontinue

let is_closed (type rw) (t : rw t) =
  match t with
  | Reader br -> Body_reader.is_closed br
  | Writer bw -> Body_writer.is_closed bw

let close_writer (Writer body_writer : [ `write ] t) =
  Body_writer.close body_writer
;;

let close_reader (Reader body_reader : [ `read ] t) =
  Body_reader.close body_reader
;;

let schedule_read (Reader body_reader : [ `read ] t) ~on_eof ~on_read =
  Body_reader.schedule_read body_reader ~on_eof ~on_read
