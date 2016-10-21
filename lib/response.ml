type t =
  { version : Version.t
  ; status  : Status.t
  ; reason  : string
  ; headers : Headers.t }

let create ?reason ?(version=Version.v1_1) ?(headers=Headers.empty) status =
  let reason =
    match reason with
    | Some reason -> reason
    | None ->
      begin match status with
      | #Status.standard as status -> Status.default_reason_phrase status
      | `Code _                    -> "Non-standard status code"
      end
  in
  { version; status; reason; headers }

let persistent_connection ?proxy { version; headers } =
  Message.persistent_connection ?proxy version headers

let proxy_error  = `Error `Bad_gateway
let server_error = `Error `Internal_server_error
let body_length ?(proxy=false) ~request_method { status; headers } =
  match status, request_method with
  | (`No_content | `Not_modified), _           -> `Fixed 0L
  | s, _        when Status.is_informational s -> `Fixed 0L
  | s, `CONNECT when Status.is_successful s    -> `Close_delimited
  | _, _                                       ->
    begin match Headers.get_multi headers "transfer-encoding" with
    | "chunked"::_                             -> `Chunked
    | _        ::es when List.mem "chunked" es -> `Close_delimited
    | [] | _                                   ->
      begin match Message.unique_content_length_values headers with
      | []      -> `Close_delimited
      | [ len ] ->
        let len = Message.content_length_of_string len in
        if Int64.(len >= 0L)
        then `Fixed len
        else if proxy then proxy_error else server_error
      | _       ->
        if proxy then proxy_error else server_error
      end
    end

let pp_hum fmt { version; status; reason; headers } =
  Format.fprintf fmt "((version \"%a\") (status %a) (reason %S) (headers %a))"
    Version.pp_hum version Status.pp_hum status reason Headers.pp_hum headers
