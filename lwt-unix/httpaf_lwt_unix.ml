(*----------------------------------------------------------------------------
    Copyright (c) 2018 Inhabited Type LLC.
    Copyright (c) 2018 Anton Bachin

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

open Lwt.Infix

let read fd buffer =
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


let shutdown socket command =
  try Lwt_unix.shutdown socket command
  with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()

module Config = Httpaf.Config

module Server = struct
  module Server_connection = Httpaf.Server_connection

  let start_read_write_loops
    ?(readf=read)
    ?(writev=Faraday_lwt_unix.writev_of_fd)
    ~config
    ~socket
    connection =
    let read_buffer = Buffer.create config.Config.read_buffer_size in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

    let rec read_loop () =
      let rec read_loop_step () =
        match Server_connection.next_read_operation connection with
        | `Read ->
          readf socket read_buffer >>= begin function
          | `Eof ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Server_connection.read_eof connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          | `Ok _ ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Server_connection.read connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          end

        | `Yield ->
          Server_connection.yield_reader connection read_loop;
          Lwt.return_unit

        | `Close ->
          Lwt.wakeup_later notify_read_loop_exited ();
          if not (Lwt_unix.state socket = Lwt_unix.Closed) then begin
            shutdown socket Unix.SHUTDOWN_RECEIVE
          end;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          read_loop_step
          (fun exn ->
            Server_connection.report_exn connection exn;
            Lwt.return_unit))
    in


    let writev = writev socket in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in

    let rec write_loop () =
      let rec write_loop_step () =
        match Server_connection.next_write_operation connection with
        | `Write io_vectors ->
          writev io_vectors >>= fun result ->
          Server_connection.report_write_result connection result;
          write_loop_step ()

        | `Yield ->
          Server_connection.yield_writer connection write_loop;
          Lwt.return_unit

        | `Close _ ->
          Lwt.wakeup_later notify_write_loop_exited ();
          if not (Lwt_unix.state socket = Lwt_unix.Closed) then begin
            shutdown socket Unix.SHUTDOWN_SEND
          end;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          write_loop_step
          (fun exn ->
            Server_connection.report_exn connection exn;
            Lwt.return_unit))
    in


    read_loop ();
    write_loop ();
    Lwt.join [read_loop_exited; write_loop_exited] >>= fun () ->

    if Lwt_unix.state socket <> Lwt_unix.Closed then
      Lwt.catch
        (fun () -> Lwt_unix.close socket)
        (fun _exn -> Lwt.return_unit)
    else
      Lwt.return_unit

  let create_connection_handler ?(config=Config.default) ~request_handler ~error_handler =
    fun client_addr socket ->
      let connection =
        Server_connection.create
          ~config
          ~error_handler:(error_handler client_addr)
          (request_handler client_addr)
      in
      start_read_write_loops ~config ~socket connection

  module TLS = struct
    let create_connection_handler
      ?server
      ?certfile
      ?keyfile
      ?(config=Config.default)
      ~request_handler
      ~error_handler =
      let make_tls_server = Tls_io.make_server ?server ?certfile ?keyfile in
      fun client_addr socket ->
        let connection =
          Server_connection.create
            ~config
            ~error_handler:(error_handler client_addr)
            (request_handler client_addr)
        in
        make_tls_server socket >>= fun tls_server ->
        let readf = Tls_io.readf tls_server in
        let writev = Tls_io.writev tls_server in
        start_read_write_loops ~config ~readf ~writev ~socket connection
        >>= Lwt.return
  end

  module SSL = struct
    let create_connection_handler
      ?server
      ?certfile
      ?keyfile
      ?(config=Config.default)
      ~request_handler
      ~error_handler =
      let make_ssl_server = Ssl_io.make_server ?server ?certfile ?keyfile in
      fun client_addr socket ->
        let connection =
          Server_connection.create
            ~config
            ~error_handler:(error_handler client_addr)
            (request_handler client_addr)
        in
        make_ssl_server socket >>= fun tls_server ->
        let readf = Ssl_io.readf tls_server in
        let writev = Ssl_io.writev tls_server in
        start_read_write_loops ~config ~readf ~writev ~socket connection
        >>= Lwt.return
  end
end



module Client = struct
  module Client_connection = Httpaf.Client_connection

  let start_read_write_loops
    ?(readf=read)
    ?(writev=Faraday_lwt_unix.writev_of_fd)
    ~config
    ~socket
    connection =
    let read_buffer = Buffer.create config.Config.read_buffer_size in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

    let read_loop () =
      let rec read_loop_step () =
        match Client_connection.next_read_operation connection with
        | `Read ->
          readf socket read_buffer >>= begin function
          | `Eof ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Client_connection.read_eof connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          | `Ok _ ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Client_connection.read connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          end

        | `Close ->
          Lwt.wakeup_later notify_read_loop_exited ();
          if not (Lwt_unix.state socket = Lwt_unix.Closed) then begin
            shutdown socket Unix.SHUTDOWN_RECEIVE
          end;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          read_loop_step
          (fun exn ->
            Client_connection.report_exn connection exn;
            Lwt.return_unit))
    in


    let writev = writev socket in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in

    let rec write_loop () =
      let rec write_loop_step () =
        match Client_connection.next_write_operation connection with
        | `Write io_vectors ->
          writev io_vectors >>= fun result ->
          Client_connection.report_write_result connection result;
          write_loop_step ()

        | `Yield ->
          Client_connection.yield_writer connection write_loop;
          Lwt.return_unit

        | `Close _ ->
          Lwt.wakeup_later notify_write_loop_exited ();
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          write_loop_step
          (fun exn ->
            Client_connection.report_exn connection exn;
            Lwt.return_unit))
    in


    read_loop ();
    write_loop ();

    Lwt.async (fun () ->
      Lwt.join [read_loop_exited; write_loop_exited] >>= fun () ->

      if Lwt_unix.state socket <> Lwt_unix.Closed then
        Lwt.catch
          (fun () -> Lwt_unix.close socket)
          (fun _exn -> Lwt.return_unit)
      else
        Lwt.return_unit)

  let request ?(config=Config.default) socket request ~error_handler ~response_handler =
    let request_body, connection =
      Client_connection.request ~config request ~error_handler ~response_handler
    in

    start_read_write_loops ~config ~socket connection;
    request_body

  module TLS = struct
    let request ?client ?(config=Config.default) socket request ~error_handler ~response_handler =
      let request_body, connection =
        Client_connection.request ~config request ~error_handler ~response_handler
      in

      Lwt.async(fun () ->
        Tls_io.make_client ?client socket >|= fun tls_client ->
        let readf = Tls_io.readf tls_client in
        let writev = Tls_io.writev tls_client in

        start_read_write_loops ~config ~readf ~writev ~socket connection);
      request_body
  end

  module SSL = struct
    let request ?client ?(config=Config.default) socket request ~error_handler ~response_handler =
      let request_body, connection =
        Client_connection.request ~config request ~error_handler ~response_handler
      in

      Lwt.async(fun () ->
        Ssl_io.make_client ?client socket >|= fun tls_client ->
        let readf = Ssl_io.readf tls_client in
        let writev = Ssl_io.writev tls_client in

        start_read_write_loops ~config ~readf ~writev ~socket connection);
      request_body
  end
end
