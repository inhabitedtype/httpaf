open Httpaf

let maybe_serialize_body f body =
  match body with
  | None -> ()
  | Some body -> Faraday.write_string f body

let request_to_string ?body r =
  let f = Faraday.create 0x1000 in
  Httpaf_private.Serialize.write_request f r;
  maybe_serialize_body f body;
  Faraday.serialize_to_string f

let response_to_string ?body r =
  let f = Faraday.create 0x1000 in
  Httpaf_private.Serialize.write_response f r;
  maybe_serialize_body f body;
  Faraday.serialize_to_string f

module Read_operation = struct
  type t = [ `Read | `Yield | `Close | `Upgrade ]

  let pp_hum fmt (t : t) =
    let str =
      match t with
      | `Read -> "Read"
      | `Yield -> "Yield"
      | `Close -> "Close"
      | `Upgrade -> "Upgrade"
    in
    Format.pp_print_string fmt str
  ;;
end

module Write_operation = struct
  type t = [ `Write of Bigstringaf.t IOVec.t list | `Yield | `Close of int | `Upgrade ]

  let iovecs_to_string iovecs =
    let len = IOVec.lengthv iovecs in
    let bytes = Bytes.create len in
    let dst_off = ref 0 in
    List.iter (fun { IOVec.buffer; off = src_off; len } ->
      Bigstringaf.unsafe_blit_to_bytes buffer ~src_off bytes ~dst_off:!dst_off ~len;
      dst_off := !dst_off + len)
    iovecs;
    Bytes.unsafe_to_string bytes
  ;;

  let pp_hum fmt (t : t) =
    match t with
    | `Write iovecs -> Format.fprintf fmt "Write %S" (iovecs_to_string iovecs)
    | `Yield -> Format.pp_print_string fmt "Yield"
    | `Close len -> Format.fprintf fmt "Close %i" len
    | `Upgrade -> Format.pp_print_string fmt "Upgrade"
  ;;

  let to_write_as_string t =
    match t with
    | `Write iovecs -> Some (iovecs_to_string iovecs)
    | `Close _ | `Yield | `Upgrade -> None
  ;;
end

let write_operation = Alcotest.of_pp Write_operation.pp_hum
let read_operation = Alcotest.of_pp Read_operation.pp_hum

module Headers = struct
  include Headers

  let (@) a b = Headers.add_list a (Headers.to_list b)

  let connection_close = Headers.of_list ["connection", "close"]
  let encoding_chunked = Headers.of_list ["transfer-encoding", "chunked"]
  let encoding_fixed n = Headers.of_list ["content-length", string_of_int n]
  let upgrade protocol = Headers.of_list ["connection", "upgrade" ; "upgrade", protocol]
end
