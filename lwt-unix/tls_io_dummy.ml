let readf _tls =
  fun _fd _buffer ->
  Lwt.fail_with "Tls not available"

let writev _tls _fd =
  fun _iovecs ->
  Lwt.fail_with "Tls not available"

type client = [ `Tls_not_available ]
type server = [ `Tls_not_available ]

let make_client ?client:_ _socket =
  Lwt.fail_with "Tls not available"

let make_server ?server:_ ?certfile:_ ?keyfile:_ _socket =
  Lwt.fail_with "Tls not available"
