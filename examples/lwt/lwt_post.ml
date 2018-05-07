module Body = Httpaf.Body
module Response = Httpaf.Response

let response_handler : unit Lwt.u -> Response.t -> [ `read ] Body.t -> unit =
    fun notify_request_finished response response_body ->

  match response.status with
  | `OK ->
    let rec read_response () =
      Body.schedule_read
        response_body
        ~on_eof:(fun () -> Lwt.wakeup_later notify_request_finished ())
        ~on_read:(fun response_fragment ~off ~len ->
          let response_fragment_string = Bytes.create len in
          Lwt_bytes.blit_to_bytes
            response_fragment off
            response_fragment_string 0
            len;
          print_string (Bytes.unsafe_to_string response_fragment_string);

          read_response ())
    in
    read_response ()

  | _ ->
    Format.fprintf Format.err_formatter "%a\n%!" Response.pp_hum response;
    exit 1

(* TODO Real error handler *)
let error_handler _ =
  assert false

open Lwt.Infix

let () =
  let host = ref None in
  let port = ref 8080 in

  Arg.parse
    ["-p", Set_int port, " port number"]
    (fun host_argument -> host := Some host_argument)
    "lwt_get.exe [-p N] HOST";

  let host =
    match !host with
    | None -> failwith "No hostname provided"
    | Some host -> host
  in

  Lwt_main.run begin
    Lwt_io.(read stdin)
    >>= fun request_content ->

    Lwt_unix.getaddrinfo host (string_of_int !port) [Unix.(AI_FAMILY PF_INET)]
    >>= fun addresses ->

    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr
    >>= fun () ->

    let headers =
      Httpaf.Headers.of_list [
        "Host", host;
        "Connection", "close";
        "Content-Length", string_of_int (String.length request_content);
      ]
    in
    let request = Httpaf.Request.create ~headers `POST "/" in
    let request_finished, notify_request_finished = Lwt.wait () in
    let request_body =
      Httpaf_lwt.Client.request
        socket
        request
        ~error_handler
        ~response_handler:(response_handler notify_request_finished)
    in
    Body.write_string request_body request_content;
    Body.close_writer request_body;

    request_finished
  end
