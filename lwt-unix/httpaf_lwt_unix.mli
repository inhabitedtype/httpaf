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

  module TLS : sig
    val create_connection_handler
      :  ?server         : Tls_io.server
      -> ?certfile       : string
      -> ?keyfile        : string
      -> ?config         : Config.t
      -> request_handler : (Unix.sockaddr -> Server_connection.request_handler)
      -> error_handler   : (Unix.sockaddr -> Server_connection.error_handler)
      -> Unix.sockaddr
      -> Lwt_unix.file_descr
      -> unit Lwt.t
  end

  module SSL : sig
    val create_connection_handler
      :  ?server         : Ssl_io.server
      -> ?certfile       : string
      -> ?keyfile        : string
      -> ?config         : Config.t
      -> request_handler : (Unix.sockaddr -> Server_connection.request_handler)
      -> error_handler   : (Unix.sockaddr -> Server_connection.error_handler)
      -> Unix.sockaddr
      -> Lwt_unix.file_descr
      -> unit Lwt.t
  end
end

(* For an example, see [examples/lwt_get.ml]. *)
module Client : sig
  val request
    :  ?config          : Config.t
    -> Lwt_unix.file_descr
    -> Request.t
    -> error_handler    : Client_connection.error_handler
    -> response_handler : Client_connection.response_handler
    -> [`write] Body.t

  module TLS : sig
    val request
      :  ?client          : Tls_io.client
      -> ?config          : Config.t
      -> Lwt_unix.file_descr
      -> Request.t
      -> error_handler    : Client_connection.error_handler
      -> response_handler : Client_connection.response_handler
      -> [`write] Body.t
  end

  module SSL : sig
    val request
      :  ?client          : Ssl_io.client
      -> ?config          : Config.t
      -> Lwt_unix.file_descr
      -> Request.t
      -> error_handler    : Client_connection.error_handler
      -> response_handler : Client_connection.response_handler
      -> [`write] Body.t
  end
end
