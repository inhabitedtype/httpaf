open Lwt.Infix

let () = Ssl.init ()

let readf socket =
  fun _fd buffer ->
  Lwt.catch
    (fun () ->
      Buffer.put buffer ~f:(fun bigstring ~off ~len ->
        Lwt_unix.blocking (Lwt_ssl.get_fd socket) >>= fun _ ->
        Lwt_ssl.read_bytes socket bigstring off len))
    (function
    | Unix.Unix_error (Unix.EBADF, _, _) as exn ->
      Lwt.fail exn
    | exn ->
      Lwt.async (fun () ->
        Lwt_ssl.ssl_shutdown socket >>= fun () ->
        Lwt_ssl.close socket);
      Lwt.fail exn)
  >>= fun bytes_read ->
    if bytes_read = 0 then
      Lwt.return `Eof
    else
      Lwt.return (`Ok bytes_read)

let writev socket _fd =
  fun iovecs ->
  Lwt.catch
    (fun () ->
      Lwt_list.fold_left_s (fun acc {Faraday.buffer; off; len} ->
        Lwt_ssl.write_bytes socket buffer off len
        >|= fun written -> acc + written) 0 iovecs
      >|= fun n -> `Ok n)
    (function
    | Unix.Unix_error (Unix.EBADF, "check_descriptor", _) ->
      Lwt.return `Closed
    | exn ->
      Lwt.fail exn)

type client = Lwt_ssl.socket
type server = Lwt_ssl.socket

let make_client ?client socket =
  match client with
  | Some client -> Lwt.return client
  | None ->
    let client_ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context in
    Ssl.disable_protocols client_ctx [Ssl.SSLv23];
    Ssl.honor_cipher_order client_ctx;
    Lwt_ssl.ssl_connect socket client_ctx

let make_server ?server ?certfile ?keyfile socket
  =
  match server, certfile, keyfile with
  | Some server, _, _ -> Lwt.return server
  | None, Some cert, Some priv_key ->
    let server_ctx = Ssl.create_context Ssl.SSLv23 Ssl.Server_context in
    Ssl.disable_protocols server_ctx [Ssl.SSLv23];
    Ssl.use_certificate server_ctx cert priv_key;
    Lwt_ssl.ssl_accept socket server_ctx
  | _ ->
    Lwt.fail (Invalid_argument "Certfile and Keyfile required when server isn't provided")

