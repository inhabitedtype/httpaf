open Httpaf
open Lwt.Infix

let string_of_pp f x =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  f ppf x;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let report_error = function
 | `Malformed_response msg ->
    Lwt_io.eprintl msg
 | `Invalid_response_body_length _ ->
    Lwt_io.eprintl "Invalid response body length."
 | `Exn exn ->
    Lwt_io.eprintl (Printexc.to_string exn)

let response_handler finished response response_body =
  Lwt.async @@ fun () ->
  Lwt_io.printl (string_of_pp Response.pp_hum response) >|= fun () ->
  let on_eof () = Lwt.wakeup_later finished (Ok ()) in
  let rec on_read bs ~off ~len =
    Lwt.async @@ fun () ->
    Lwt_io.print (Bigstring.to_string ~off ~len bs) >|= fun () ->
    Body.schedule_read response_body ~on_read ~on_eof in
  Body.schedule_read response_body ~on_read ~on_eof

let error_handler finished error = Lwt.wakeup_later finished (Error error)

let main host port =
  let host_entry = Unix.gethostbyname host in
  let sock = Lwt_unix.socket host_entry.Unix.h_addrtype Lwt_unix.SOCK_STREAM 0 in
  let errors = ref [] in
  let process () =
    let request_promise, request_resolver = Lwt.wait () in
    let headers = Headers.of_list [
      "Host", host;
    ] in
    let request_body =
      Httpaf_lwt_unix.Client.request
        ~error_handler:(error_handler request_resolver)
        ~response_handler:(response_handler request_resolver)
        sock
        (Request.create ~headers `GET "/") in
    Body.close_writer request_body;
    request_promise in
  let rec connect_loop i =
    if i = Array.length host_entry.h_addr_list then begin
      Lwt_io.eprintl "Address unreachable." >>= fun () ->
      Lwt_list.iter_s
        (fun (inet_addr, msg) ->
          Lwt_io.eprintlf "%s: %s"
            (Unix.string_of_inet_addr inet_addr) msg)
        (List.rev !errors) >>= fun () ->
      exit 69
    end else begin
      let inet_addr = host_entry.h_addr_list.(i) in
      let sockaddr = Lwt_unix.ADDR_INET (inet_addr, port) in
      Lwt.catch
        (fun () ->
          Lwt_unix.connect sock sockaddr >>= fun () ->
          process ())
        (function
         | Unix.Unix_error (error, _, _) ->
            errors := (inet_addr, Unix.error_message error) :: !errors;
            connect_loop (i + 1)
         | exn ->
            Lwt.return_error (`Exn exn))
    end in
  connect_loop 0 >>=
  (function
   | Ok () -> Lwt.return 0
   | Error error -> report_error error >|= fun () -> 69)

let () =
  let host = ref "" in
  let port = ref 80 in
  Arg.parse
    ["-a", Arg.Set_string host, " Address.";
     "-p", Arg.Set_int port, " Port number."]
    (fun _ ->
      prerr_endline "No positional arguments accepted.";
      exit 64)
    "lwt_unix_get -a ADDRESS [-p PORT]";
  if !host = "" then begin
    prerr_endline "The -a option is mandatory unless you just ask for -help.";
    exit 64
  end else
    exit (Lwt_main.run (main !host !port))
