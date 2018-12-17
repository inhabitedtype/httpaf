open Lwt.Infix
open Httpaf

module Dispatch (C: Mirage_types_lwt.CONSOLE) = struct

  let log c fmt = Printf.ksprintf (C.log c) fmt

  let get_content c path =
    log c "Replying: %s" path >|= fun () ->
    "Hello from the httpaf unikernel"

  let dispatcher c reqd =
    let {Request.target; _} = Reqd.request reqd in
    Lwt.catch
      (fun () ->
         get_content c target >|= fun body ->
         let response = Response.create
          ~headers:(Headers.of_list ["Content-Length", body
            |> String.length
            |> string_of_int])
          `OK
         in
         Reqd.respond_with_string reqd response body)
      (fun exn ->
         let response = Response.create `Internal_server_error in
         Lwt.return (Reqd.respond_with_string reqd response (Printexc.to_string exn)))
    |> ignore

  let serve c dispatch =
    let error_handler ?request:_ _error mk_response =
      let response_body = mk_response Headers.empty in
      Body.write_string response_body "Error handled";
      Body.flush response_body (fun () -> Body.close_writer response_body)
    in
    Httpaf_mirage.Server.create_connection_handler
      ~config:(Server_connection.Config.default)
      ~request_handler:(dispatch c)
      ~error_handler
end

module type HTTP = sig
  type t = (Conduit_mirage.Flow.flow -> unit Lwt.t)

  val connect:
    Conduit_mirage.t ->
    (Conduit_mirage.server -> t -> unit Lwt.t) Lwt.t
end

(** Server boilerplate *)
module Make (C : Mirage_types_lwt.CONSOLE) (Clock : Mirage_types_lwt.PCLOCK) (Http: HTTP) = struct

  module D  = Dispatch (C)

  let log c fmt = Printf.ksprintf (C.log c) fmt
  let start c _clock http =
    log c "started unikernel listen on port 8001" >>= fun () ->
    http (`TCP 8001) @@ D.serve c D.dispatcher
end