open Core
open Async
open Async_ssl

module Unix = Core.Unix


let readf ssl_reader =
  fun _fd buffer ->
  Buffer.put_async buffer ~f:(fun bigstring ~off ~len ->
    let bigsubstr = Bigsubstring.create ~pos:off ~len bigstring in
    Reader.read_bigsubstring ssl_reader bigsubstr >>| function
      | `Eof -> 0
      | `Ok n -> n)
  >>| function
  | 0 -> `Eof
  | n -> `Ok n

let writev ssl_writer _fd =
  fun iovecs ->
    let iovecs_q = Queue.create ~capacity:(List.length iovecs) () in
    let len = List.fold ~init:0 ~f:(fun acc { Faraday.buffer; off = pos; len } ->
      Queue.enqueue iovecs_q (Unix.IOVec.of_bigstring ~pos ~len buffer);
      acc + len) iovecs
    in
    Writer.schedule_iovecs ssl_writer iovecs_q;
    Writer.flushed ssl_writer
    >>| fun () -> `Ok len

let close_read ssl_reader = fun _socket ->
  Reader.close ssl_reader

let close_write ssl_writer = fun _socket ->
  Writer.close ssl_writer

type client = Reader.t * Writer.t
type server = Reader.t * Writer.t

let reader (r, _) = r
let writer (_, w) = w

(* taken from https://github.com/janestreet/async_extra/blob/master/src/tcp.ml *)
let reader_writer_of_sock ?buffer_age_limit ?reader_buffer_size ?writer_buffer_size s =
  let fd = Socket.fd s in
  ( Reader.create ?buf_len:reader_buffer_size fd
  , Writer.create ?buffer_age_limit ?buf_len:writer_buffer_size fd )

let connect r w =
  let net_to_ssl = Reader.pipe r in
  let ssl_to_net = Writer.pipe w in
  let app_to_ssl, app_wr = Pipe.create () in
  let app_rd, ssl_to_app = Pipe.create () in
  Ssl.client
    ~app_to_ssl
    ~ssl_to_app
    ~net_to_ssl
    ~ssl_to_net
    ()
  |> Deferred.Or_error.ok_exn
  >>= fun conn ->
    Reader.of_pipe (Info.of_string "httpaf_async_ssl_reader") app_rd >>= fun app_reader ->
    Writer.of_pipe (Info.of_string "httpaf_async_ssl_writer") app_wr >>| fun (app_writer,_) ->
    don't_wait_for begin
      Deferred.all_unit [
        Writer.close_finished app_writer ;
        Reader.close_finished app_reader ;
      ] >>= fun () ->
      Ssl.Connection.close conn ;
      Pipe.close_read app_rd ;
      Writer.close w ;
    end ;
    (app_reader, app_writer)

let make_client ?client socket =
  match client with
  | Some client -> Deferred.return client
  | None ->
    let reader, writer = reader_writer_of_sock socket in
    connect reader writer

let listen ~crt_file ~key_file r w =
  let net_to_ssl = Reader.pipe r in
  let ssl_to_net = Writer.pipe w in
  let app_to_ssl, app_wr = Pipe.create () in
  let app_rd, ssl_to_app = Pipe.create () in
  Ssl.server
    ~crt_file
    ~key_file
    ~app_to_ssl
    ~ssl_to_app
    ~net_to_ssl
    ~ssl_to_net
    ()
  |> Deferred.Or_error.ok_exn
  >>= fun conn ->
  Reader.of_pipe (Info.of_string "httpaf_async_ssl_reader") app_rd >>= fun app_reader ->
  Writer.of_pipe (Info.of_string "httpaf_async_ssl_writer") app_wr >>| fun (app_writer,_) ->
  don't_wait_for begin
    Deferred.all_unit [
      Reader.close_finished app_reader;
      Writer.close_finished app_writer
    ] >>= fun () ->
    Ssl.Connection.close conn ;
    Pipe.close_read app_rd ;
    Writer.close w ;
  end;
  (app_reader, app_writer)

let make_server ?server ?certfile ?keyfile socket =
  match server, certfile, keyfile with
  | Some server, _, _ -> Deferred.return server
  | None, Some crt_file, Some key_file ->
    let reader, writer = reader_writer_of_sock socket in
    listen ~crt_file ~key_file reader writer
  | _ ->
    failwith "Certfile and Keyfile required when server isn't provided"
