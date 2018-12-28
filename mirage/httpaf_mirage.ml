open Lwt.Infix

(* Based on the Buffer module in httpaf_async.ml. *)
module Buffer : sig
  type t

  val create : int -> t

  val get : t -> f:(Bigstringaf.t -> off:int -> len:int -> int) -> int
  val put : t -> f:(Bigstringaf.t -> off:int -> len:int -> int Lwt.t) -> int Lwt.t
end = struct
  type t =
    { buffer      : Bigstringaf.t
    ; mutable off : int
    ; mutable len : int }

  let create size =
    let buffer = Bigstringaf.create size in
    { buffer; off = 0; len = 0 }

  let compress t =
    if t.len = 0
    then begin
      t.off <- 0;
      t.len <- 0;
    end else if t.off > 0
    then begin
      Bigstringaf.blit t.buffer ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len;
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
    f t.buffer ~off:(t.off + t.len) ~len:(Bigstringaf.length t.buffer - t.len)
    >>= fun n ->
    t.len <- t.len + n;
    Lwt.return n
end

let shutdown flow =
  Lwt.async (fun () -> Conduit_mirage.Flow.close flow)

let read flow buffer =
  let open Conduit_mirage in
  Lwt.catch
    (fun () ->
      Buffer.put buffer ~f:(fun bigstring ~off ~len:_ ->
        Flow.read flow >|= function
        | Ok (`Data buf) ->
          Bigstringaf.blit buf.buffer ~src_off:buf.off bigstring ~dst_off:off ~len:buf.len;
          buf.len
        | Ok `Eof -> 0
        | Error error -> raise (Failure (Format.asprintf "%a" Flow.pp_error error))))
    (fun exn ->
      shutdown flow;
      Lwt.fail exn)

  >>= fun bytes_read ->
  if bytes_read = 0 then
    Lwt.return `Eof
  else
    Lwt.return (`Ok bytes_read)

let writev_of_flow flow =
  fun iovecs ->
    let open Conduit_mirage in
    let cstruct_iovecs = List.map (fun {Faraday.buffer; off; len} ->
      Cstruct.of_bigarray ~off ~len buffer)
      iovecs
    in

    Lwt.catch
      (fun () ->
        Flow.writev flow cstruct_iovecs >|= fun x ->
        match x with
        | Ok () ->
          let written = List.fold_left (fun acc {Cstruct.len; off; _} ->
            acc + (len - off))
            0 cstruct_iovecs
          in
          `Ok written
        | Error `Closed ->
          `Closed
        | Error other_error ->
          raise (Failure (Format.asprintf "%a" Flow.pp_write_error other_error)))
      (fun exn ->
        shutdown flow;
        Lwt.fail exn)

module Server = struct
  type request_handler =
    Conduit_mirage.Flow.flow Httpaf.Server_connection.request_handler


  let create_connection_handler ?config ~request_handler ~error_handler () =
    fun flow ->
      let module Server_connection = Httpaf.Server_connection in
      let connection =
        Server_connection.create
          ?config
          ~error_handler
          request_handler
      in


      let read_buffer = Buffer.create 0x1000 in
      let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

      let rec read_loop () =
        let rec read_loop_step () =
          match Server_connection.next_read_operation connection with
          | `Read ->
            read flow read_buffer >>= begin function
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
            shutdown flow;
            Lwt.return_unit
        in

        Lwt.async (fun () ->
          Lwt.catch
            read_loop_step
            (fun exn ->
              Server_connection.report_exn connection exn;
              Lwt.return_unit))
      in


      let writev = writev_of_flow flow in
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
            shutdown flow;
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

      Lwt.catch
        (fun () -> Conduit_mirage.Flow.close flow)
        (fun _exn -> Lwt.return_unit)
end

module type Server_intf = sig
  type request_handler =
    Conduit_mirage.Flow.flow Httpaf.Server_connection.request_handler

  val create_connection_handler
    :  ?config : Httpaf.Server_connection.Config.t
    -> request_handler : request_handler
    -> error_handler : Httpaf.Server_connection.error_handler
    -> unit
    -> (Conduit_mirage.Flow.flow -> unit Lwt.t)
end

module Server_with_conduit = struct
  open Conduit_mirage

  include Server

  type t = Conduit_mirage.Flow.flow -> unit Lwt.t

  let listen handler flow =
      Lwt.finalize
        (fun () -> handler flow)
        (fun () -> Flow.close flow)

  let connect t =
    let listen s f = Conduit_mirage.listen t s (listen f) in
    Lwt.return listen
end



module Client = struct
  let request flow request ~error_handler ~response_handler =
    let module Client_connection = Httpaf.Client_connection in
    let request_body, connection =
      Client_connection.request request ~error_handler ~response_handler in


    let read_buffer = Buffer.create 0x1000 in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

    let read_loop () =
      let rec read_loop_step () =
        match Client_connection.next_read_operation connection with
        | `Read ->
          read flow read_buffer >>= begin function
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
          shutdown flow;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          read_loop_step
          (fun exn ->
            Client_connection.report_exn connection exn;
            Lwt.return_unit))
    in


    let writev = writev_of_flow flow in
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

      Lwt.catch
        (fun () -> Conduit_mirage.Flow.close flow)
        (fun _exn -> Lwt.return_unit));

    request_body
end
