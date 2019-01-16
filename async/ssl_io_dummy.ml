let readf _socket =
  fun _fd _buffer ->
  failwith "Ssl not available"

let writev _socket _fd =
  fun _iovecs ->
  failwith "Ssl not available"

let close_read _ssl_reader = fun _socket ->
  failwith "Ssl not available"

let close_write _ssl_writer = fun _socket ->
  failwith "Ssl not available"

type client = [ `Ssl_not_available  ]
type server = [ `Ssl_not_available  ]

let reader _ =
  failwith "Ssl not available"

let writer _ =
  failwith "Ssl not available"

let make_client ?client:_ _socket =
  failwith "Ssl not available"

let make_server ?server:_ ?certfile:_ ?keyfile:_ _socket =
  failwith "Ssl not available"
