open Async
open Httpaf

module Server : sig
  module Upgrade : sig
    type 'a t =
      | Ignore
      | Raise
      | Handle of (([`Active], 'a) Socket.t -> Request.t -> Response.t -> unit Deferred.t)
  end

  val create_connection_handler
    :  ?config         : Config.t
    -> upgrade_handler : 'a Upgrade.t
    -> request_handler : ('a -> Server_connection.request_handler)
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
