open Base
open Httpaf
module Format = Caml.Format

let print_string = Stdio.(Out_channel.output_string stdout)

module Client = struct
  let print ~on_eof response response_body =
    match response with
    | { Response.status = `OK; _ } as response ->
      Format.fprintf Format.std_formatter "%a\n%!" Response.pp_hum response;
      let rec on_read bs ~off ~len =
        Bigstringaf.substring ~off ~len bs |> print_string;
        Body.schedule_read response_body ~on_read ~on_eof
      in
      Body.schedule_read response_body ~on_read ~on_eof;
    | response ->
      Format.fprintf Format.err_formatter "%a\n%!" Response.pp_hum response;
      Caml.exit 1
  ;;
end

module Server = struct
  let echo_post reqd =
    match Reqd.request reqd  with
    | { Request.meth = `POST; headers; _ } ->
      let response =
        let content_type =
          match Headers.get headers "content-type" with
          | None   -> "application/octet-stream"
          | Some x -> x
        in
        Response.create ~headers:(Headers.of_list ["content-type", content_type; "connection", "close"]) `OK
      in
      let request_body  = Reqd.request_body reqd in
      let response_body = Reqd.respond_with_streaming reqd response in
      let rec on_read buffer ~off ~len =
        Body.write_bigstring response_body buffer ~off ~len;
        Body.schedule_read request_body ~on_eof ~on_read;
      and on_eof () =
        Body.close_writer response_body
      in
      Body.schedule_read (Reqd.request_body reqd) ~on_eof ~on_read
    | _ ->
      let headers = Headers.of_list [ "connection", "close" ] in
      Reqd.respond_with_string reqd (Response.create ~headers `Method_not_allowed) ""
  ;;

  let error_handler ?request:_ error start_response =
    let response_body = start_response Headers.empty in
    begin match error with
    | `Exn exn ->
      Body.write_string response_body (Exn.to_string exn);
      Body.write_string response_body "\n";
    | #Status.standard as error ->
      Body.write_string response_body (Status.default_reason_phrase error)
    end;
    Body.close_writer response_body
  ;;
end
