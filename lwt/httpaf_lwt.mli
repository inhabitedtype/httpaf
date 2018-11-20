(* The function that results from [create_connection_handler] should be passed
   to [Lwt_io.establish_server_with_client_socket]. For an example, see
   [examples/lwt_echo_server.ml]. *)
module Server : sig
  type request_handler =
    Lwt_unix.file_descr Httpaf.Server_connection.request_handler

  val create_connection_handler
    :  ?config : Httpaf.Server_connection.Config.t
    -> request_handler : (Unix.sockaddr -> request_handler)
    -> error_handler : (Unix.sockaddr -> Httpaf.Server_connection.error_handler)
      -> (Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t)
end

(* For an example, see [examples/lwt_get.ml]. *)
module Client : sig
  val request
    :  Lwt_unix.file_descr
    -> Httpaf.Request.t
    -> error_handler : Httpaf.Client_connection.error_handler
    -> response_handler : Httpaf.Client_connection.response_handler
      -> [`write] Httpaf.Body.t
end
