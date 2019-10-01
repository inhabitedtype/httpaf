open Lwt.Infix

module Make_IO (Flow: Mirage_flow_lwt.S) :
  Httpaf_lwt.IO with type socket = Flow.flow and type addr = unit = struct
  type socket = Flow.flow
  type addr = unit

  let shutdown flow =
    Flow.close flow

  let shutdown_receive flow =
    Lwt.async (fun () -> shutdown flow)

  let shutdown_send flow =
    Lwt.async (fun () -> shutdown flow)

  let close flow = shutdown flow

  let read flow bigstring ~off ~len:_ =
    Lwt.catch
      (fun () ->
        Flow.read flow >|= function
        | Ok (`Data buf) ->
          Bigstringaf.blit
            buf.buffer
            ~src_off:buf.off bigstring
            ~dst_off:off
            ~len:buf.len;
          `Ok buf.len
        | Ok `Eof -> `Eof
        | Error error ->
          raise (Failure (Format.asprintf "%a" Flow.pp_error error)))
      (fun exn ->
        shutdown flow >>= fun () ->
        Lwt.fail exn)

  let writev flow = fun iovecs ->
      let cstruct_iovecs = List.map (fun {Faraday.buffer; off; len} ->
        Cstruct.of_bigarray ~off ~len buffer)
        iovecs
      in

      Lwt.catch
        (fun () ->
          Flow.writev flow cstruct_iovecs >|= fun x ->
          match x with
          | Ok () ->
            `Ok (Cstruct.lenv cstruct_iovecs)
          | Error `Closed ->
            `Closed
          | Error other_error ->
            raise (Failure (Format.asprintf "%a" Flow.pp_write_error other_error)))
        (fun exn ->
          shutdown flow >>= fun () ->
          Lwt.fail exn)
end

module Server (Flow : Mirage_flow_lwt.S) = struct
  include Httpaf_lwt.Server (Make_IO (Flow))

  type flow = Flow.flow

  let create_connection_handler ?config ~request_handler ~error_handler =
    fun flow ->
      let request_handler = fun () -> request_handler in
      let error_handler = fun () -> error_handler in
      create_connection_handler ?config ~request_handler ~error_handler () flow
end

module type Server_intf = sig
  type flow

  val create_connection_handler
    :  ?config : Httpaf.Config.t
    -> request_handler : Httpaf.Server_connection.request_handler
    -> error_handler : Httpaf.Server_connection.error_handler
    -> (flow -> unit Lwt.t)
end

module Server_with_conduit = struct
  open Conduit_mirage
  include Server (Conduit_mirage.Flow)

  type t = Conduit_mirage.Flow.flow -> unit Lwt.t

  let listen handler flow =
    Lwt.finalize
      (fun () -> handler flow)
      (fun () -> Flow.close flow)

  let connect t =
    let listen s f = Conduit_mirage.listen t s (listen f) in
    Lwt.return listen
end

module Client (Flow : Mirage_flow_lwt.S) = Httpaf_lwt.Client (Make_IO (Flow))
