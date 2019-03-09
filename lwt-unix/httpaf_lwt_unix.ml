open Lwt.Infix

module Io
  : Httpaf_lwt.IO with
    type t = Unix.sockaddr * Lwt_unix.file_descr = struct
  module Buffer = Httpaf_lwt.Buffer
  type t = Unix.sockaddr * Lwt_unix.file_descr

   let read (_, fd) buffer =
    Lwt.catch
      (fun () ->
        Buffer.put buffer ~f:(fun bigstring ~off ~len ->
          Lwt_bytes.read fd bigstring off len))
      (function
      | Unix.Unix_error (Unix.EBADF, _, _) as exn ->
        Lwt.fail exn
      | exn ->
        Lwt.async (fun () ->
          Lwt_unix.close fd);
        Lwt.fail exn)

     >>= fun bytes_read ->
    if bytes_read = 0 then
      Lwt.return `Eof
    else
      Lwt.return (`Ok bytes_read)

   let writev (_, fd) = Faraday_lwt_unix.writev_of_fd fd

   let shutdown socket command =
    try Lwt_unix.shutdown socket command
    with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()

   let shutdown_send (_, socket) =
    if not (Lwt_unix.state socket = Lwt_unix.Closed) then
      shutdown socket Unix.SHUTDOWN_SEND

   let shutdown_receive (_, socket) =
    if not (Lwt_unix.state socket = Lwt_unix.Closed) then
      shutdown socket Unix.SHUTDOWN_RECEIVE

   let close (_, socket) =
    if Lwt_unix.state socket <> Lwt_unix.Closed then
      Lwt.catch
        (fun () -> Lwt_unix.close socket)
        (fun _exn -> Lwt.return_unit)
    else
      Lwt.return_unit
end

 module Server = struct
  include Httpaf_lwt.Server (Io)

   let create_connection_handler ?config ~request_handler ~error_handler =
    fun client_addr file_descr ->
      create_connection_handler ?config ~request_handler ~error_handler (client_addr, file_descr)
end

 module Client = struct
  include Httpaf_lwt.Client (Io)

   let request ?config socket req ~error_handler ~response_handler =
    let addr = Unix.getsockname (Lwt_unix.unix_file_descr socket) in
    request ?config (addr, socket) req ~error_handler ~response_handler
end
