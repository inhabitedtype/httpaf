open Lwt.Infix
open Httpaf

module type HTTP = Httpaf_mirage.Server_intf

module Dispatch (C: Mirage_types_lwt.CONSOLE) (Http: HTTP) = struct

  let log c fmt = Printf.ksprintf (C.log c) fmt

  let get_content c path =
    log c "Replying: %s" path >|= fun () ->
    "Hello from the httpaf unikernel"

  let dispatcher c flow reqd =
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
    let error_handler flow ?request:_ _error mk_response =
      let response_body = mk_response Headers.empty in
      Body.write_string response_body "Error handled";
      Body.flush response_body (fun () -> Body.close_writer response_body)
    in
    Http.create_connection_handler
      ?config:None
      ~request_handler:(dispatch c)
      ~error_handler
end

(** Server boilerplate *)
module Make (C : Mirage_types_lwt.CONSOLE) (Clock : Mirage_types_lwt.PCLOCK) (Http: HTTP) = struct

  module D  = Dispatch (C) (Http)

  let log c fmt = Printf.ksprintf (C.log c) fmt
  let start c _clock http =
    log c "started unikernel listen on port 8001" >>= fun () ->
    http (`TCP 8001) @@ D.serve c D.dispatcher
end
