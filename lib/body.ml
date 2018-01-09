type _ t =
  { faraday                     : Faraday.t
  ; mutable scheduled           : bool
  ; mutable on_eof              : unit -> unit
  ; mutable on_read             : Bigstring.t -> off:int -> len:int -> unit
  ; mutable when_ready_to_write : unit -> unit
  ; buffered_bytes              : int ref
  }

let default_on_eof         = Sys.opaque_identity (fun () -> ())
let default_on_read        = Sys.opaque_identity (fun _ ~off:_ ~len:_ -> ())
let default_ready_to_write = Sys.opaque_identity (fun () -> ())

let of_faraday faraday =
  { faraday
  ; scheduled = false
  ; on_eof    = default_on_eof
  ; on_read   = default_on_read
  ; when_ready_to_write = default_ready_to_write
  ; buffered_bytes = ref 0
  }

let create buffer =
  of_faraday (Faraday.of_bigstring buffer)

let create_empty () =
  let t = create Bigstring.empty in
  Faraday.close t.faraday;
  t

let empty = create_empty ()

let write_char t c =
  Faraday.write_char t.faraday c

let write_string t ?off ?len s =
  Faraday.write_string ?off ?len t.faraday s

let write_bigstring t ?off ?len b =
  Faraday.write_bigstring ?off ?len t.faraday b

let schedule_bigstring t ?off ?len (b:Bigstring.t) =
  Faraday.schedule_bigstring ?off ?len t.faraday b

let ready_to_write t =
  let callback = t.when_ready_to_write in
  t.when_ready_to_write <- default_ready_to_write;
  callback ()

let flush t kontinue =
  Faraday.flush t.faraday kontinue;
  ready_to_write t

let is_closed t =
  Faraday.is_closed t.faraday

let close t =
  Faraday.close t.faraday;
  ready_to_write t

let unsafe_faraday t =
  t.faraday

let rec do_execute_read t on_eof on_read =
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
    Faraday.shift t.faraday len;
    on_read buffer ~off ~len;
    execute_read t
and execute_read t =
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

let has_pending_output t =
  Faraday.has_pending_output t.faraday

let when_ready_to_write t callback =
  if is_closed t then callback ();
  if not (t.when_ready_to_write == default_ready_to_write)
  then failwith "Body.when_ready_to_write: only one callback can be registered at a time";
  t.when_ready_to_write <- callback

let transfer_to_writer_with_encoding t ~encoding writer =
  let faraday = t.faraday in
  begin match Faraday.operation faraday with
  | `Yield | `Close -> ()
  | `Writev iovecs ->
    let buffered = t.buffered_bytes in
    let iovecs   = IOVec.shiftv  iovecs !buffered in
    let lengthv  = IOVec.lengthv iovecs in
    buffered := !buffered + lengthv;
    begin match encoding with
    | `Fixed _ | `Close_delimited -> Serialize.Writer.schedule_fixed writer iovecs
    | `Chunked                    -> Serialize.Writer.schedule_chunk writer iovecs
    end;
    Serialize.Writer.flush writer (fun () ->
      Faraday.shift faraday lengthv;
      buffered := !buffered - lengthv)
  end
