open Lwt.Infix

module Io
  : Httpaf_lwt.IO with
    type socket = Lwt_unix.file_descr
    and type addr = Unix.sockaddr = struct
  type socket = Lwt_unix.file_descr
  type addr = Unix.sockaddr

  let read socket bigstring ~off ~len =
    Lwt.catch
      (fun () -> Lwt_bytes.read socket bigstring off len)
      (function
      | Unix.Unix_error (Unix.EBADF, _, _) as exn ->
        Lwt.fail exn
      | exn ->
        Lwt.async (fun () ->
          Lwt_unix.close socket);
        Lwt.fail exn)

     >>= fun bytes_read ->
    if bytes_read = 0 then
      Lwt.return `Eof
    else
      Lwt.return (`Ok bytes_read)

  let writev socket = Faraday_lwt_unix.writev_of_fd socket

   let shutdown socket command =
    try Lwt_unix.shutdown socket command
    with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()

  let shutdown_send socket =
    if not (Lwt_unix.state socket = Lwt_unix.Closed) then
      shutdown socket Unix.SHUTDOWN_SEND

  let shutdown_receive socket =
    if not (Lwt_unix.state socket = Lwt_unix.Closed) then
      shutdown socket Unix.SHUTDOWN_RECEIVE

  let close socket =
    if Lwt_unix.state socket <> Lwt_unix.Closed then
      Lwt.catch
        (fun () -> Lwt_unix.close socket)
        (fun _exn -> Lwt.return_unit)
    else
      Lwt.return_unit
end

module Server = Httpaf_lwt.Server (Io)

module Client = Httpaf_lwt.Client (Io)
