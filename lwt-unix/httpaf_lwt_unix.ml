(*----------------------------------------------------------------------------
    Copyright (c) 2018 Inhabited Type LLC.

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
open Httpaf

module Server = struct
  let create_connection_handler ?config ~request_handler ~error_handler =
    fun client_addr sock ->

    let conn =
      Server_connection.create ?config
        ~error_handler:(error_handler client_addr)
        (request_handler client_addr) in

    let launch_io f = Lwt.async @@ fun () ->
      Lwt.catch f
        (fun exn -> Server_connection.report_exn conn exn; Lwt.return_unit) in

    let reader_done, release_read = Lwt.wait () in
    let writer_done, release_write = Lwt.wait () in

    let buffer = Lwt_bytes.create 0x1000 in (* TODO: Make configurable. *)
    let rec reader_thread avail_off avail_end =
      match Server_connection.next_read_operation conn with
      | `Read ->
        if avail_off < avail_end then
          let len = avail_end - avail_off in
          let consumed_len =
            Server_connection.read conn buffer ~off:avail_off ~len in
          reader_thread (avail_off + consumed_len) avail_end
        else begin
          Lwt_bytes.read sock buffer 0 (Lwt_bytes.length buffer) >>= fun len ->
          if len = 0 then begin
            Server_connection.shutdown_reader conn;
            reader_thread 0 0
          end else begin
            let consumed_len = Server_connection.read conn buffer ~off:0 ~len in
            reader_thread consumed_len len
          end
        end
      | `Yield ->
        Server_connection.yield_reader conn
          (fun () -> launch_io (fun () -> reader_thread avail_off avail_end));
        Lwt.return_unit
      | `Close ->
        if Lwt_unix.state sock = Lwt_unix.Opened then
          (try Lwt_unix.shutdown sock Lwt_unix.SHUTDOWN_RECEIVE with
           Unix.Unix_error (Unix.ENOTCONN, _, _) -> ());
        Lwt.wakeup_later release_read ();
        Lwt.return_unit in
    launch_io (fun () -> reader_thread 0 0);

    let rec writer_thread () =
      match Server_connection.next_write_operation conn with
      | `Write iovecs ->
        Faraday_lwt_unix.writev_of_fd sock iovecs >>= fun result ->
        Server_connection.report_write_result conn result;
        writer_thread ()
      | `Yield ->
        Server_connection.yield_writer conn (fun () -> launch_io writer_thread);
        Lwt.return_unit
      | `Close _ ->
        if Lwt_unix.state sock = Lwt_unix.Opened then
          (try Lwt_unix.shutdown sock Lwt_unix.SHUTDOWN_SEND with
           Unix.Unix_error (Unix.ENOTCONN, _, _) -> ());
        Lwt.wakeup_later release_write ();
        Lwt.return_unit in
    launch_io writer_thread;

    Lwt.join [writer_done; reader_done] >>= fun () ->
    Lwt_unix.close sock

end

module Client = struct

  let request sock request ~error_handler ~response_handler =

    let request_body, conn =
      Client_connection.request request ~error_handler ~response_handler in

    let launch_io f = Lwt.async @@ fun () ->
      Lwt.catch f
        (fun exn -> Client_connection.report_exn conn exn; Lwt.return_unit) in

    let reader_done, release_read = Lwt.wait () in
    let writer_done, release_write = Lwt.wait () in

    let buffer = Lwt_bytes.create 0x1000 in (* TODO: Make configurable. *)
    let rec reader_thread avail_off avail_end =
      match Client_connection.next_read_operation conn with
      | `Read ->
        if avail_off < avail_end then
          let len = avail_end - avail_off in
          let consumed_len =
            Client_connection.read conn buffer ~off:avail_off ~len in
          reader_thread (avail_off + consumed_len) avail_end
        else begin
          Lwt_bytes.read sock buffer 0 (Lwt_bytes.length buffer) >>= fun len ->
          if len = 0 then begin
            Client_connection.shutdown_reader conn;
            reader_thread 0 0
          end else begin
            let consumed_len = Client_connection.read conn buffer ~off:0 ~len in
            reader_thread consumed_len len
          end
        end
      | `Close ->
        if Lwt_unix.state sock = Lwt_unix.Opened then
          (try Lwt_unix.shutdown sock Lwt_unix.SHUTDOWN_RECEIVE with
           Unix.Unix_error (Unix.ENOTCONN, _, _) -> ());
        Lwt.wakeup_later release_read ();
        Lwt.return_unit in
    launch_io (fun () -> reader_thread 0 0);

    let rec writer_thread () =
      match Client_connection.next_write_operation conn with
      | `Write iovecs ->
        Faraday_lwt_unix.writev_of_fd sock iovecs >>= fun result ->
        Client_connection.report_write_result conn result;
        writer_thread ()
      | `Yield ->
        Client_connection.yield_writer conn (fun () -> launch_io writer_thread);
        Lwt.return_unit
      | `Close _ ->
        if Lwt_unix.state sock = Lwt_unix.Opened then
          (try Lwt_unix.shutdown sock Lwt_unix.SHUTDOWN_SEND with
           Unix.Unix_error (Unix.ENOTCONN, _, _) -> ());
        Lwt.wakeup_later release_write ();
        Lwt.return_unit in
    launch_io writer_thread;

    Lwt.async begin fun () ->
      Lwt.join [writer_done; reader_done] >>= fun () ->
      Lwt_unix.close sock
    end;
    request_body

end
