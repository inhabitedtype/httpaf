open Lwt.Infix

module Io : Httpaf_lwt.IO with type t = Conduit_mirage.Flow.flow = struct
  module Buffer = Httpaf_lwt.Buffer
  type t = Conduit_mirage.Flow.flow

  let shutdown flow =
    Conduit_mirage.Flow.close flow

  let shutdown_receive flow =
    Lwt.async (fun () -> shutdown flow)

  let shutdown_send flow =
    Lwt.async (fun () -> shutdown flow)

  let close flow = shutdown flow

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
        shutdown flow >>= fun () ->
        Lwt.fail exn)

    >>= fun bytes_read ->
    if bytes_read = 0 then
      Lwt.return `Eof
    else
      Lwt.return (`Ok bytes_read)

  let writev flow = fun iovecs ->
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
          shutdown flow >>= fun () ->
          Lwt.fail exn)
end

module Server = Httpaf_lwt.Server (Io)

module type Server_intf = sig
  val create_connection_handler
    :  ?config : Httpaf.Config.t
    -> request_handler :
        (Conduit_mirage.Flow.flow -> Httpaf.Server_connection.request_handler)
    -> error_handler :
        (Conduit_mirage.Flow.flow -> Httpaf.Server_connection.error_handler)
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



module Client = Httpaf_lwt.Client (Io)
