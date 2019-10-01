let readf _socket =
  fun _fd _buffer ->
  Lwt.fail_with "Ssl not available"

let writev _socket _fd =
  fun _iovecs ->
  Lwt.fail_with "Ssl not available"

type client = [ `Ssl_not_available  ]
type server = [ `Ssl_not_available  ]

let[@ocaml.warning "-21"] make_client ?client:_ =
  failwith "Ssl not available";
  fun _socket -> Lwt.fail_with "Ssl not available"

let[@ocaml.warning "-21"] make_server ?server:_ ?certfile:_ ?keyfile:_ =
  failwith "Ssl not available";
  fun _socket -> Lwt.fail_with "Ssl not available"
