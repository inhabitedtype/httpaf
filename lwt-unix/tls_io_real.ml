open Lwt.Infix

let _ = Nocrypto_entropy_lwt.initialize ()

let readf tls =
  fun _fd buffer ->
  Lwt.catch
    (fun () ->
      Buffer.put buffer ~f:(fun bigstring ~off ~len ->
        Tls_lwt.Unix.read_bytes tls bigstring off len))
    (function
    | Unix.Unix_error (Unix.EBADF, _, _) as exn ->
        Lwt.fail exn
    | exn ->
      Lwt.async (fun () ->
        Tls_lwt.Unix.close tls);
      Lwt.fail exn)
  >>= fun bytes_read ->
  if bytes_read = 0 then
    Lwt.return `Eof
  else
    Lwt.return (`Ok bytes_read)

let writev tls _fd =
  fun iovecs ->
  Lwt.catch
    (fun () ->
      let cstruct_iovecs = List.map (fun {Faraday.len; buffer; off} ->
        Cstruct.of_bigarray ~off ~len buffer)
        iovecs
      in
      Tls_lwt.Unix.writev tls cstruct_iovecs
      >|= fun () ->
        `Ok (Cstruct.lenv cstruct_iovecs))
    (function
    | Unix.Unix_error (Unix.EBADF, "check_descriptor", _) ->
      Lwt.return `Closed
    | exn -> Lwt.fail exn)

type client = Tls_lwt.Unix.t
type server = Tls.Config.server

let make_client ?client socket =
  match client with
  | Some client -> Lwt.return client
  | None ->
    X509_lwt.authenticator `No_authentication_I'M_STUPID >>= fun authenticator ->
    let config = Tls.Config.client ~authenticator () in
    Tls_lwt.Unix.client_of_fd config socket

let make_server ?server ?certfile ?keyfile socket
  =
  let config = match server, certfile, keyfile with
  | Some server, _, _ -> Lwt.return server
  | None, Some cert, Some priv_key ->
    X509_lwt.private_of_pems ~cert ~priv_key >>= fun certificate ->
    X509_lwt.authenticator `No_authentication_I'M_STUPID >|= fun authenticator ->
    Tls.Config.server
      ~certificates:(`Single certificate)
      ~authenticator
      ()
  | _ ->
    Lwt.fail (Invalid_argument "Certfile and Keyfile required when server isn't provided")
  in
  config >>= fun config -> Tls_lwt.Unix.server_of_fd config socket


