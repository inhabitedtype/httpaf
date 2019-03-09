open Httpaf

module Buffer : sig
  type t

  val create : int -> t

  val get : t -> f:(Bigstringaf.t -> off:int -> len:int -> int) -> int
  val put : t -> f:(Bigstringaf.t -> off:int -> len:int -> int Lwt.t) -> int Lwt.t
end

module type IO = sig
  type t

  val read : t -> Buffer.t -> [> `Eof | `Ok of int ] Lwt.t

  val writev
     : t
    -> Faraday.bigstring Faraday.iovec list
    -> [ `Closed | `Ok of int ] Lwt.t

  val shutdown_send : t -> unit

  val shutdown_receive : t -> unit

  val close : t -> unit Lwt.t
end

(* The function that results from [create_connection_handler] should be passed
   to [Lwt_io.establish_server_with_client_socket]. For an example, see
   [examples/lwt_echo_server.ml]. *)
module Server (Io: IO) : sig
  val create_connection_handler
    :  ?config         : Config.t
    -> request_handler : (Io.t -> Server_connection.request_handler)
    -> error_handler   : (Io.t -> Server_connection.error_handler)
    -> Io.t
    -> unit Lwt.t
end

(* For an example, see [examples/lwt_get.ml]. *)
module Client (Io: IO) : sig
  val request
    :  ?config          : Httpaf.Config.t
    -> Io.t
    -> Request.t
    -> error_handler    : Client_connection.error_handler
    -> response_handler : Client_connection.response_handler
    -> [`write] Httpaf.Body.t
end
