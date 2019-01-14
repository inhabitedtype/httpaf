module Body = Httpaf.Body

let response_handler notify_response_received response response_body =
  let module Response = Httpaf.Response in
  match Response.(response.status) with
  | `OK ->
    let rec read_response () =
      Body.schedule_read
        response_body
        ~on_eof:(fun () -> Lwt.wakeup_later notify_response_received ())
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

let error_handler _ =
  assert false

open Lwt.Infix

let () =
  let host = ref None in
  let port = ref 443 in

  Arg.parse
    ["-p", Set_int port, " Port number (443 by default)"]
    (fun host_argument -> host := Some host_argument)
    "lwt_https_get.exe [-p N] HOST";

  let host =
    match !host with
    | None -> failwith "No hostname provided"
    | Some host -> host
  in

  Lwt_main.run begin
    Lwt_unix.getaddrinfo host (string_of_int !port) [Unix.(AI_FAMILY PF_INET)]
    >>= fun addresses ->

    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr
    >>= fun () ->

    let request_headers =
      Httpaf.Request.create
        `GET "/" ~headers:(Httpaf.Headers.of_list ["Host", host])
    in

    let response_received, notify_response_received = Lwt.wait () in
    let response_handler = response_handler notify_response_received in

    let request_body =
      Httpaf_lwt.Client.request_tls
        socket
        request_headers
        ~error_handler
        ~response_handler
    in
    Body.close_writer request_body;

    response_received
  end
