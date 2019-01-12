open Httpaf


(* The function that results from [create_connection_handler] should be passed
   to [Lwt_io.establish_server_with_client_socket]. For an example, see
   [examples/lwt_echo_server.ml]. *)
module Server : sig
  val create_connection_handler
    :  ?config         : Config.t
    -> request_handler : (Unix.sockaddr -> Server_connection.request_handler)
    -> error_handler   : (Unix.sockaddr -> Server_connection.error_handler)
    -> Unix.sockaddr
    -> Lwt_unix.file_descr
    -> unit Lwt.t
end

(* For an example, see [examples/lwt_get.ml]. *)
module Client : sig
  val request
    :  ?config          : Httpaf.Config.t
    -> Lwt_unix.file_descr
    -> Request.t
    -> error_handler    : Client_connection.error_handler
    -> response_handler : Client_connection.response_handler
    -> [`write] Httpaf.Body.t
end
