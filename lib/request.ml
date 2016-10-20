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
