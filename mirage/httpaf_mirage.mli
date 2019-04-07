module type Server_intf = sig
  type flow

  val create_connection_handler
    :  ?config : Httpaf.Config.t
    -> request_handler : Httpaf.Server_connection.request_handler
    -> error_handler : Httpaf.Server_connection.error_handler
    -> (flow -> unit Lwt.t)
end

module Server (Flow : Mirage_flow_lwt.S) :
  Server_intf with type flow = Flow.flow

module Server_with_conduit : sig
  include Server_intf with type flow = Conduit_mirage.Flow.flow

  type t = Conduit_mirage.Flow.flow -> unit Lwt.t

  val connect:
    Conduit_mirage.t ->
    (Conduit_mirage.server -> t -> unit Lwt.t) Lwt.t
end

module Client (Flow : Mirage_flow_lwt.S) : sig
  val request
    :  ?config : Httpaf.Config.t
    -> Flow.flow
    -> Httpaf.Request.t
    -> error_handler : Httpaf.Client_connection.error_handler
    -> response_handler : Httpaf.Client_connection.response_handler
      -> [`write] Httpaf.Body.t
end
