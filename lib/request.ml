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
  { meth    : Method.t
  ; target  : string
  ; version : Version.t
  ; headers : Headers.t }

let create ?(version=Version.v1_1) ?(headers=Headers.empty) meth target =
  { meth; target; version; headers }

let bad_requst = `Error `Bad_request
let body_length { headers; _ } =
  (* XXX(seliopou): perform proper transfer-encoding parsing *)
  match Headers.get_multi headers "transfer-encoding" with
  | "chunked"::_                             -> `Chunked
  | _        ::es when List.mem "chunked" es -> `Error `Bad_request
  | [] | _                                   ->
    begin match Message.unique_content_length_values headers with
    | []      -> `Fixed 0L
    | [ len ] ->
      let len = Message.content_length_of_string len in
      if len >= 0L
      then `Fixed len
      else bad_requst
    | _       -> bad_requst
    end

let persistent_connection ?proxy { version; headers; _ } =
  Message.persistent_connection ?proxy version headers

let pp_hum fmt { meth; target; version; headers } =
  Format.fprintf fmt "((method \"%a\") (target %S) (version \"%a\") (headers %a))"
    Method.pp_hum meth target Version.pp_hum version Headers.pp_hum headers

module Body = struct
  type bigstring = Bigstring.t
  type 'a iovec = 'a IOVec.t

  type t =
    { faraday            : Faraday.t
    ; mutable scheduled  : bool
    ; mutable on_eof     : unit -> unit
    ; mutable on_read    : Bigstring.t -> off:int -> len:int -> int
    }

  let default_on_eof  = Sys.opaque_identity (fun () -> ())
  let default_on_read = Sys.opaque_identity (fun _ ~off:_ ~len:_ -> -1)

  let create buffer =
    { faraday   = Faraday.of_bigstring buffer
    ; scheduled = false
    ; on_eof    = default_on_eof
    ; on_read   = default_on_read
    }

  let create_empty () =
    let t = create Bigstring.empty in
    Faraday.close t.faraday;
    t

  let empty = create_empty ()

  let is_closed t =
    Faraday.is_closed t.faraday

  let close t =
    Faraday.close t.faraday

  let has_pending_output t =
    Faraday.has_pending_output t.faraday

  let unsafe_faraday t =
    t.faraday

  let do_execute_read t on_eof on_read =
    match Faraday.operation t.faraday with
    | `Yield           -> ()
    | `Close           ->
      t.scheduled <- false;
      t.on_eof    <- default_on_eof;
      t.on_read   <- default_on_read;
      on_eof ()
    | `Writev []       -> assert false
    | `Writev (iovec::_) ->
      t.scheduled <- false;
      t.on_eof    <- default_on_eof;
      t.on_read   <- default_on_read;
      let { IOVec.buffer; off; len } = iovec in
      let n = on_read buffer ~off ~len in
      assert (n >= 0);
      Faraday.shift t.faraday n

  let execute_read t =
    if t.scheduled then do_execute_read t t.on_eof t.on_read

  let schedule_read t ~on_eof ~on_read =
    if t.scheduled
    then failwith "Body.schedule_read: reader already scheduled";
    if is_closed t
    then do_execute_read t on_eof on_read
    else begin
      t.scheduled <- true;
      t.on_eof    <- on_eof;
      t.on_read   <- on_read
    end
end
