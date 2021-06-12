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

module Reader = struct
  type t =
    { faraday                        : Faraday.t
    ; mutable read_scheduled         : bool
    ; mutable on_eof                 : unit -> unit
    ; mutable on_read                : Bigstringaf.t -> off:int -> len:int -> unit
    }

  let default_on_eof         = Sys.opaque_identity (fun () -> ())
  let default_on_read        = Sys.opaque_identity (fun _ ~off:_ ~len:_ -> ())

  let create buffer =
    { faraday                = Faraday.of_bigstring buffer
    ; read_scheduled         = false
    ; on_eof                 = default_on_eof
    ; on_read                = default_on_read
    }

  let create_empty () =
    let t = create Bigstringaf.empty in
    Faraday.close t.faraday;
    t

  let empty = create_empty ()

  let is_closed t =
    Faraday.is_closed t.faraday

  let unsafe_faraday t =
    t.faraday

  let rec do_execute_read t on_eof on_read =
    match Faraday.operation t.faraday with
    | `Yield           -> ()
    | `Close           ->
      t.read_scheduled <- false;
      t.on_eof         <- default_on_eof;
      t.on_read        <- default_on_read;
      on_eof ()
    (* [Faraday.operation] never returns an empty list of iovecs *)
    | `Writev []       -> assert false
    | `Writev (iovec::_) ->
      t.read_scheduled <- false;
      t.on_eof         <- default_on_eof;
      t.on_read        <- default_on_read;
      let { IOVec.buffer; off; len } = iovec in
      Faraday.shift t.faraday len;
      on_read buffer ~off ~len;
      execute_read t
  and execute_read t =
    if t.read_scheduled then do_execute_read t t.on_eof t.on_read

  let schedule_read t ~on_eof ~on_read =
    if t.read_scheduled
    then failwith "Body.Reader.schedule_read: reader already scheduled";
    if not (is_closed t) then begin
      t.read_scheduled <- true;
      t.on_eof         <- on_eof;
      t.on_read        <- on_read;
    end;
    do_execute_read t on_eof on_read

  let close t =
    Faraday.close t.faraday;
    execute_read t
  ;;

  let has_pending_output t = Faraday.has_pending_output t.faraday
end

module Writer = struct
  type encoding =
    | Identity
    | Chunked of { mutable written_final_chunk : bool }

  type t =
    { faraday             : Faraday.t
    ; encoding            : encoding
    ; when_ready_to_write : unit -> unit
    ; buffered_bytes      : int ref
    }

  let of_faraday faraday ~encoding ~when_ready_to_write =
    let encoding =
      match encoding with
      | `Fixed _ | `Close_delimited -> Identity
      | `Chunked -> Chunked { written_final_chunk = false }
    in
    { faraday
    ; encoding
    ; when_ready_to_write
    ; buffered_bytes = ref 0
    }

  let create buffer ~encoding ~when_ready_to_write =
    of_faraday (Faraday.of_bigstring buffer) ~encoding ~when_ready_to_write

  let write_char t c =
    Faraday.write_char t.faraday c

  let write_string t ?off ?len s =
    Faraday.write_string ?off ?len t.faraday s

  let write_bigstring t ?off ?len b =
    Faraday.write_bigstring ?off ?len t.faraday b

  let schedule_bigstring t ?off ?len (b:Bigstringaf.t) =
    Faraday.schedule_bigstring ?off ?len t.faraday b

  let ready_to_write t = t.when_ready_to_write ()

  let flush t kontinue =
    Faraday.flush t.faraday kontinue;
    ready_to_write t

  let is_closed t =
    Faraday.is_closed t.faraday

  let close t =
    Faraday.close t.faraday;
    ready_to_write t;
  ;;

  let has_pending_output t =
    (* Force another write poll to make sure that the final chunk is emitted for
       chunk-encoded bodies. *)
    let faraday_has_output = Faraday.has_pending_output t.faraday in
    let additional_encoding_output =
      match t.encoding with
      | Identity -> false
      | Chunked { written_final_chunk } ->
        Faraday.is_closed t.faraday && not written_final_chunk
    in
    faraday_has_output || additional_encoding_output

  let transfer_to_writer t writer =
    let faraday = t.faraday in
    begin match Faraday.operation faraday with
    | `Yield -> ()
    | `Close ->
      (match t.encoding with
       | Identity -> ()
       | Chunked ({ written_final_chunk } as chunked) ->
         if not written_final_chunk then begin
           chunked.written_final_chunk <- true;
           Serialize.Writer.schedule_chunk writer [];
         end);
      Serialize.Writer.unyield writer;
    | `Writev iovecs ->
      let buffered = t.buffered_bytes in
      begin match IOVec.shiftv iovecs !buffered with
      | []     -> ()
      | iovecs ->
        let lengthv  = IOVec.lengthv iovecs in
        buffered := !buffered + lengthv;
        begin match t.encoding with
        | Identity  -> Serialize.Writer.schedule_fixed writer iovecs
        | Chunked _ -> Serialize.Writer.schedule_chunk writer iovecs
        end;
        Serialize.Writer.flush writer (fun () ->
          Faraday.shift faraday lengthv;
          buffered := !buffered - lengthv)
      end
    end
end
