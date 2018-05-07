(* TODO Document the server is meant to be used with establish_server', and
whatever client is meant to be used with. *)
(* TODO Explain where exceptions go and how to wrap the server callback. *)
(* TODO Local usage examples, or refer people to the example. *)

module Server : sig
  type request_handler =
    Lwt_unix.file_descr Httpaf.Server_connection.request_handler

  val create_connection_handler
    :  ?config:Httpaf.Server_connection.Config.t
    -> request_handler:(Unix.sockaddr -> request_handler)
    -> error_handler:(Unix.sockaddr -> Httpaf.Server_connection.error_handler)
      -> (Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t)
end

module Client : sig
  val request
    :  Lwt_unix.file_descr
    -> Httpaf.Request.t
    -> error_handler : Httpaf.Client_connection.error_handler
    -> response_handler : Httpaf.Client_connection.response_handler
      -> [`write] Httpaf.Body.t
end
