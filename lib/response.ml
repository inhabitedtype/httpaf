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

open Sexplib.Std

type t =
  { version : Version.t
  ; status  : Status.t
  ; reason  : string
  ; headers : Headers.t }
[@@deriving sexp]

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

let persistent_connection ?proxy { version; headers; _ } =
  Message.persistent_connection ?proxy version headers

let proxy_error  = `Error `Bad_gateway
let server_error = `Error `Internal_server_error

module Body_length = struct
  type t = [
    | `Fixed of Int64.t
    | `Chunked
    | `Close_delimited
    | `Error of [ `Bad_gateway | `Internal_server_error ]
  ]

  let pp_hum fmt (len : t) =
    match len with
    | `Fixed n -> Format.fprintf fmt "Fixed %Li" n
    | `Chunked -> Format.pp_print_string fmt "Chunked"
    | `Close_delimited -> Format.pp_print_string fmt "Close delimited"
    | `Error `Bad_gateway -> Format.pp_print_string fmt "Error: Bad gateway"
    | `Error `Internal_server_error ->
        Format.pp_print_string fmt "Error: Internal server error"
  ;;
end

let body_length ?(proxy=false) ~request_method { status; headers; _ } : Body_length.t =
  match status, request_method with
  | _, `HEAD                                   -> `Fixed 0L
  | (`No_content | `Not_modified), _           -> `Fixed 0L
  | s, _        when Status.is_informational s -> `Fixed 0L
  | s, `CONNECT when Status.is_successful s    -> `Close_delimited
  | _, _                                       ->
    (* The last entry in transfer-encoding is the correct entry. We only handle
       chunked transfer-encodings. *)
    begin match List.rev (Headers.get_multi headers "transfer-encoding") with
    | value::_ when Headers.ci_equal value "chunked" -> `Chunked
    | _    ::_ -> `Close_delimited
    | [] ->
      begin match Message.unique_content_length_values headers with
      | []      -> `Close_delimited
      | [ len ] ->
        let len = Message.content_length_of_string len in
        if len >= 0L
        then `Fixed len
        else if proxy then proxy_error else server_error
      | _       ->
        if proxy then proxy_error else server_error
      end
    end

let pp_hum fmt { version; status; reason; headers } =
  Format.fprintf fmt "((version \"%a\") (status %a) (reason %S) (headers %a))"
    Version.pp_hum version Status.pp_hum status reason Headers.pp_hum headers
