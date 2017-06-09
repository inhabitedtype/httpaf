type t =
  { meth    : Method.t
  ; target  : string
  ; version : Version.t
  ; headers : Headers.t }

let create ?(version=Version.v1_1) ?(headers=Headers.empty) meth target =
  { meth; target; version; headers }

let bad_requst = `Error `Bad_request
let body_length { headers } =
  (* XXX(seliopou): perform proper transfer-encoding parsing *)
  match Headers.get_multi headers "transfer-encoding" with
  | "chunked"::_                             -> `Chunked
  | _        ::es when List.mem "chunked" es -> `Error `Bad_request
  | [] | _                                   ->
    begin match Message.unique_content_length_values headers with
    | []      -> `Fixed 0L
    | [ len ] ->
      let len = Message.content_length_of_string len in
      if Int64.(len >= 0L)
      then `Fixed len
      else bad_requst
    | _       -> bad_requst
    end

let persistent_connection ?proxy { version; headers } =
  Message.persistent_connection ?proxy version headers

let pp_hum fmt { meth; target; version; headers } =
  Format.fprintf fmt "((method \"%a\") (target %S) (version \"%a\") (headers %a))"
    Method.pp_hum meth target Version.pp_hum version Headers.pp_hum headers

module Body = struct
  type bigstring = Bigstring.t
  type buffer = IOVec.buffer
  type 'a iovec = 'a IOVec.t

  type read_state =
    | Waiting   : read_state
    | Scheduled : (buffer iovec list -> 'a * int) * ([`Eof | `Ok of 'a] -> unit) -> read_state

  type t =
    { faraday            : Faraday.t
    ; mutable read_state : read_state
    }

  let create buffer =
    { faraday = Faraday.of_bigstring buffer
    ; read_state = Waiting
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

  let _execute_read t readv result =
    match Faraday.operation t.faraday with
    | `Yield         -> ()
    | `Close         -> t.read_state <- Waiting; result `Eof
    | `Writev iovecs ->
      t.read_state <- Waiting;
      let a, n = readv iovecs in
      assert (n >= 0);
      Faraday.shift t.faraday n;
      result (`Ok a)

  let execute_read t =
    match t.read_state with
    | Waiting                  -> ()
    | Scheduled(readv, result) -> _execute_read t readv result

  let schedule_read t ~readv ~result =
    match t.read_state with
    | Scheduled _ -> raise (Failure "Body.schedule_read: reader already scheduled")
    | Waiting     ->
      if is_closed t
      then _execute_read t readv result
      else t.read_state <- Scheduled(readv, result)
end
