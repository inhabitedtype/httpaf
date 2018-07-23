open Httpaf

module Server : sig
  open Server_connection

  val create_connection_handler
    :  ?config         : Config.t
    -> request_handler : (Unix.sockaddr -> Lwt_unix.file_descr request_handler)
    -> error_handler   : (Unix.sockaddr -> error_handler)
    -> Unix.sockaddr
    -> Lwt_unix.file_descr
    -> unit Lwt.t
end

module Client : sig
  val request
    :  Lwt_unix.file_descr
    -> Request.t
    -> error_handler    : Client_connection.error_handler
    -> response_handler : Client_connection.response_handler
    -> [`write] Body.t
end
