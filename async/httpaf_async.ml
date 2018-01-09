open Core
open Async

let read fd buffer =
  let badfd fd = failwithf "read got back fd: %s" (Fd.to_string fd) () in
  let rec finish fd buffer result =
    let open Unix.Error in
    match result with
    | `Already_closed | `Ok 0 -> return `Eof
    | `Ok n                   -> return (`Ok n)
    | `Error (Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
      begin Fd.ready_to fd `Read
      >>= function
        | `Bad_fd -> badfd fd
        | `Closed -> return `Eof
        | `Ready  -> go fd buffer
      end
    | `Error (Unix.Unix_error (EBADF, _, _)) ->
      badfd fd
    | `Error exn ->
      Deferred.don't_wait_for (Fd.close fd);
      raise exn
  and go fd buffer  =
    if Fd.supports_nonblock fd then
      finish fd buffer
        (Fd.syscall fd ~nonblocking:true
          (fun file_descr ->
            Unix.Syscall_result.Int.ok_or_unix_error_exn ~syscall_name:"read"
              (Bigstring.read_assume_fd_is_nonblocking file_descr buffer)))
    else
      Fd.syscall_in_thread fd ~name:"read"
        (fun file_descr -> Bigstring.read file_descr buffer)
      >>= fun result -> finish fd buffer result
  in
  go fd buffer


open Httpaf

module Server = struct
  let create_connection_handler ?config ~request_handler ~error_handler =
    fun client_addr socket ->
      let fd     = Socket.fd socket in
      let writev = Faraday_async.writev_of_fd fd in
      let request_handler = request_handler client_addr in
      let error_handler   = error_handler client_addr in
      let conn = Server_connection.create ?config ~error_handler request_handler in
      let read_complete = Ivar.create () in
      let rec reader_thread () =
        match Server_connection.next_read_operation conn with
        | `Read buffer ->
          (* Log.Global.printf "read(%d)%!" (Fd.to_int_exn fd); *)
          read fd buffer >>> fun result ->
            Server_connection.report_read_result conn result;
            reader_thread ()
        | `Yield  ->
          (* Log.Global.printf "read_yield(%d)%!" (Fd.to_int_exn fd); *)
          Server_connection.yield_reader conn reader_thread
        | `Close ->
          (* Log.Global.printf "read_close(%d)%!" (Fd.to_int_exn fd); *)
          Ivar.fill read_complete ();
          if not (Fd.is_closed fd)
          then Socket.shutdown socket `Receive
      in
      let write_complete = Ivar.create () in
      let rec writer_thread () =
        match Server_connection.next_write_operation conn with
        | `Write iovecs ->
          (* Log.Global.printf "write(%d)%!" (Fd.to_int_exn fd); *)
          writev iovecs >>> fun result ->
            Server_connection.report_write_result conn result;
            writer_thread ()
        | `Yield ->
          (* Log.Global.printf "write_yield(%d)%!" (Fd.to_int_exn fd); *)
          Server_connection.yield_writer conn writer_thread;
        | `Close _ ->
          (* Log.Global.printf "write_close(%d)%!" (Fd.to_int_exn fd); *)
          Ivar.fill write_complete ();
          if not (Fd.is_closed fd)
          then Socket.shutdown socket `Send
      in
      let conn_monitor = Monitor.create () in
      Scheduler.within ~monitor:conn_monitor reader_thread;
      Scheduler.within ~monitor:conn_monitor writer_thread;
      Monitor.detach_and_iter_errors conn_monitor ~f:(fun exn ->
        Server_connection.shutdown conn;
        Log.Global.error "%s" (Exn.to_string exn);
        if not (Fd.is_closed fd)
        then don't_wait_for (Fd.close fd));
      (* The Tcp module will close the file descriptor once this becomes determined. *)
      Deferred.all_ignore
        [ Ivar.read read_complete
        ; Ivar.read write_complete ]
end

module Client = struct
  let request socket request ~error_handler ~response_handler =
    let fd     = Socket.fd socket in
    let writev = Faraday_async.writev_of_fd fd in
    let request_body, conn   =
      Client_connection.request request ~error_handler ~response_handler in
    let read_complete = Ivar.create () in
    let rec reader_thread () =
      match Client_connection.next_read_operation conn with
      | `Read buffer ->
        (* Log.Global.printf "read(%d)%!" (Fd.to_int_exn fd); *)
        read fd buffer >>> fun result ->
          Client_connection.report_read_result conn result;
          reader_thread ()
      | `Close ->
        (* Log.Global.printf "read_close(%d)%!" (Fd.to_int_exn fd); *)
        Ivar.fill read_complete ();
        if not (Fd.is_closed fd)
        then Socket.shutdown socket `Receive
    in
    let write_complete = Ivar.create () in
    let rec writer_thread () =
      match Client_connection.next_write_operation conn with
      | `Write iovecs ->
        (* Log.Global.printf "write(%d)%!" (Fd.to_int_exn fd); *)
        writev iovecs >>> fun result ->
          Client_connection.report_write_result conn result;
          writer_thread ()
      | `Yield ->
        (* Log.Global.printf "write_yield(%d)%!" (Fd.to_int_exn fd); *)
        Client_connection.yield_writer conn writer_thread;
      | `Close _ ->
        (* Log.Global.printf "write_close(%d)%!" (Fd.to_int_exn fd); *)
        Ivar.fill write_complete ();
        if not (Fd.is_closed fd)
        then Socket.shutdown socket `Send
    in
    let conn_monitor = Monitor.create () in
    Scheduler.within ~monitor:conn_monitor reader_thread;
    Scheduler.within ~monitor:conn_monitor writer_thread;
    Monitor.detach_and_iter_errors conn_monitor ~f:(fun exn ->
      Client_connection.shutdown conn;
      Log.Global.error "%s" (Exn.to_string exn);
      if not (Fd.is_closed fd)
      then don't_wait_for (Fd.close fd));
    don't_wait_for (
      Deferred.all_ignore
        [ Ivar.read read_complete
        ; Ivar.read write_complete ]
      >>| fun () ->
        if not (Fd.is_closed fd)
        then don't_wait_for (Fd.close fd));
    request_body
end
