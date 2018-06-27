(* TODO Need a buffer module? *)
(* TODO Look into a possible one in Angstrom. *)
(* TODO Yes, need to provide own buffer. Copy the one from Httpaf_async. *)
(* TODO Fix all the whitespace and evetything *)
(* TODO There are too many copies here, because Lwt_io is already buffered. *)

open Lwt.Infix



(* TODO Note where this came from. *)
(* TODO What to do about cleanup exceptions in both server and client? *)
module Buffer : sig
  type t

  val create   : int -> t

  val get : t -> f:(Lwt_bytes.t -> off:int -> len:int -> int) -> int
  val put : t -> f:(Lwt_bytes.t -> off:int -> len:int -> int Lwt.t) -> int Lwt.t
end = struct
  type t =
    { buffer      : Lwt_bytes.t
    ; mutable off : int
    ; mutable len : int }

  let create size =
    let buffer = Lwt_bytes.create size in
    { buffer; off = 0; len = 0 }

  let compress t =
    if t.len = 0
    then begin
      t.off <- 0;
      t.len <- 0;
    end else if t.off > 0
    then begin
      Lwt_bytes.blit t.buffer t.off t.buffer 0 t.len;
      t.off <- 0;
    end

  let get t ~f =
    let n = f t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0
    then t.off <- 0;
    n

  let put t ~f =
    compress t;
    f t.buffer ~off:(t.off + t.len) ~len:(Lwt_bytes.length t.buffer - t.len)
    >>= fun n ->
    t.len <- t.len + n;
    Lwt.return n
end

(* TODO Should probably send exceptions into notify_read_complete. *)
let read fd buffer =
  Lwt.catch
    (fun () ->
      Buffer.put buffer ~f:(fun bigstring ~off ~len ->
        Lwt_bytes.read fd bigstring off len))
    (function
    | Unix.Unix_error (Unix.EBADF, _, _) as exn ->
      raise exn
    | exn ->
      Lwt.async (fun () ->
        Lwt_unix.close fd);
      raise exn)

  >>= fun bytes_read ->
  if bytes_read = 0 then
    Lwt.return `Eof
  else
    Lwt.return (`Ok bytes_read)



let shutdown socket command =
  try Lwt_unix.shutdown socket command
  with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()



(* TODO But is this really awkward? We just need a finalize call on the joined
   promise. *)
(* TODO Close the server's client connection, even though Lwt_io will also close
   it. This is for better error handling. *)
(* TODO Get exceptions passed to the error handler? *)

module Server = struct
  type request_handler =
    Lwt_unix.file_descr Httpaf.Server_connection.request_handler



  let create_connection_handler ?config ~request_handler ~error_handler =
    fun client_addr socket ->
      let module Server_connection = Httpaf.Server_connection in
      let connection =
        Server_connection.create
          ?config
          ~error_handler:(error_handler client_addr)
          (request_handler client_addr)
      in


      let read_buffer = Buffer.create 0x1000 in
      let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

      (* TODO Explain loops and steps. *)
      let rec read_loop () =
        let rec read_loop_step () =
          match Server_connection.next_read_operation connection with
          | `Read ->
            read socket read_buffer >>= begin function
            | `Eof ->
              Server_connection.shutdown_reader connection;
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


      let writev = Faraday_lwt_unix.writev_of_fd socket in
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

      let handler_finished = Lwt.join [read_loop_exited; write_loop_exited] in

      Lwt.on_failure handler_finished begin fun _exn ->
        Server_connection.shutdown connection;
        if not (Lwt_unix.state socket = Lwt_unix.Closed) then
          Lwt.async (fun () ->
            Lwt_unix.close socket)
      end;

      handler_finished
end



module Client = struct
  let request socket request ~error_handler ~response_handler =
    let module Client_connection = Httpaf.Client_connection in
    let request_body, connection =
      Client_connection.request request ~error_handler ~response_handler in


    let read_buffer = Buffer.create 0x1000 in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

    let read_loop () =
      let rec read_loop_step () =
        match Client_connection.next_read_operation connection with
        | `Read ->
          read socket read_buffer >>= begin function
          | `Eof ->
            Client_connection.shutdown_reader connection;
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


    let writev = Faraday_lwt_unix.writev_of_fd socket in
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
          if not (Lwt_unix.state socket = Lwt_unix.Closed) then begin
            shutdown socket Unix.SHUTDOWN_SEND
          end;
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

    let handler_finished = Lwt.join [read_loop_exited; write_loop_exited] in

    Lwt.on_failure handler_finished begin fun _exn ->
      Client_connection.shutdown connection;
      if not (Lwt_unix.state socket = Lwt_unix.Closed) then
        Lwt.async (fun () ->
          Lwt_unix.close socket)
    end;

    Lwt.on_success handler_finished begin fun () ->
      if not (Lwt_unix.state socket = Lwt_unix.Closed) then
        Lwt.async (fun () ->
          Lwt_unix.close socket)
    end;

    request_body
end
