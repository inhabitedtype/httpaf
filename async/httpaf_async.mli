open! Core
open Async

open Httpaf

module Server : sig
  val create_connection_handler
    :  ?config         : Config.t
    -> request_handler : ('a -> Server_connection.request_handler)
    -> ?upgrade_handler : ('a -> ([`Active], 'a) Socket.t -> Server_connection.upgrade_handler)
    -> error_handler   : ('a -> Server_connection.error_handler)
    -> ([< Socket.Address.t] as 'a)
    -> ([`Active], 'a) Socket.t
    -> unit Deferred.t
end

module Client : sig
  val request
    :  ?config          : Config.t
    -> ([`Active], [< Socket.Address.t]) Socket.t
    -> Request.t
    -> error_handler    : Client_connection.error_handler
    -> response_handler : Client_connection.response_handler
    -> [`write] Body.t
end
