open Httpaf
open Helpers

let trace fmt = Format.ksprintf (Format.printf "%s\n%!") fmt

let request_error_pp_hum fmt = function
  | `Bad_request           -> Format.fprintf fmt "Bad_request"
  | `Bad_gateway           -> Format.fprintf fmt "Bad_gateway"
  | `Internal_server_error -> Format.fprintf fmt "Internal_server_error"
  | `Exn exn               -> Format.fprintf fmt "Exn (%s)" (Printexc.to_string exn)
;;

module Alcotest = struct
  include Alcotest

  let request_error = Alcotest.of_pp request_error_pp_hum

  let request = Alcotest.of_pp (fun fmt req ->
    Format.fprintf fmt "%s" (request_to_string req))
  ;;
end

module Runtime : sig
  type t

  val create
    :  ?config:Config.t
    -> ?error_handler:Server_connection.error_handler
    -> Server_connection.request_handler
    -> t

  val current_read_operation : t -> Read_operation.t
  val current_write_operation : t -> Write_operation.t

  val do_read : t -> (Server_connection.t -> 'a) -> 'a
  val do_write : t -> (Server_connection.t -> Bigstringaf.t IOVec.t list -> 'a) -> 'a

  (** Returns a [ref] that is set to [true] after the callback was fired *)
  val on_reader_unyield : t -> (unit -> unit) -> bool ref
  val on_writer_unyield : t -> (unit -> unit) -> bool ref

  val report_exn : t -> exn -> unit

  val shutdown : t -> unit
end = struct
  open Server_connection

  type t =
    { server_connection : Server_connection.t
    ; mutable read_operation : [`Initial | Read_operation.t]
    ; mutable write_operation : [`Initial | Write_operation.t]
    ; read_loop : (unit -> unit)
    ; write_loop : (unit -> unit)
    ; mutable read_unyield_hook : (unit -> unit) option
    ; mutable write_unyield_hook : (unit -> unit) option
    }

  let rec read_step t =
    match next_read_operation t.server_connection with
    | `Read ->
      trace "reader: Read";
      t.read_operation <- `Read
    | `Yield ->
      trace "reader: Yield";
      t.read_operation <- `Yield;
      yield_reader t.server_connection (fun () ->
        trace "reader: Yield callback";
        read_step t;
        t.read_unyield_hook |> Option.iter (fun f ->
          t.read_unyield_hook <- None;
          f ()))
    | `Close ->
      trace "reader: Close";
      t.read_operation <- `Close
  ;;

  let rec write_step t =
    match next_write_operation t.server_connection with
    | `Write xs ->
      trace "writer: Write";
      t.write_operation <- `Write xs
    | `Yield ->
      t.write_operation <- `Yield;
      trace "writer: Yield";
      yield_writer t.server_connection (fun () ->
        trace "writer: Yield callback";
        write_step t;
        t.write_unyield_hook |> Option.iter (fun f ->
          t.write_unyield_hook <- None;
          f ()))
    | `Close n ->
      trace "writer: Close";
      t.write_operation <- `Close n
  ;;

  let create ?config ?error_handler request_handler =
    let request_handler r =
      trace "invoked: request_handler";
      request_handler r
    in
    let error_handler =
      Option.map (fun error_handler ?request ->
        trace "invoked: error_handler";
        error_handler ?request) error_handler
    in
    let rec t =
      lazy (
        { server_connection = create ?config ?error_handler request_handler
        ; read_operation = `Initial
        ; write_operation = `Initial
        ; read_loop = (fun () -> read_step (Lazy.force_val t))
        ; write_loop = (fun () -> write_step (Lazy.force_val t))
        ; read_unyield_hook = None
        ; write_unyield_hook = None
        })
    in
    let t = Lazy.force_val t in
    t.read_loop ();
    t.write_loop ();
    t
  ;;

  let current_read_operation t =
    match t.read_operation with
    | `Initial -> assert false
    | `Read | `Yield | `Close as op -> op
  ;;

  let current_write_operation t =
    match t.write_operation with
    | `Initial -> assert false
    | `Write _ | `Yield | `Close _ as op -> op
  ;;

  let do_read t f =
    match current_read_operation t with
    | `Read ->
      trace "read: start";
      let res = f t.server_connection in
      trace "read: finished";
      t.read_loop ();
      res
    | `Yield | `Close as op ->
      Alcotest.failf "Read attempted during operation: %a"
        Read_operation.pp_hum op
  ;;

  let do_write t f =
    match current_write_operation t with
    | `Write bufs ->
      trace "write: start";
      let res = f t.server_connection bufs in
      trace "write: finished";
      t.write_loop ();
      res
    | `Yield | `Close _ as op ->
      Alcotest.failf "Write attempted during operation: %a"
        Write_operation.pp_hum op
  ;;

  let on_reader_unyield t f =
    let called = ref false in
    assert (Option.is_none t.read_unyield_hook);
    t.read_unyield_hook <- Some (fun () -> called := true; f ());
    called
  ;;

  let on_writer_unyield t f =
    let called = ref false in
    assert (Option.is_none t.write_unyield_hook);
    t.write_unyield_hook <- Some (fun () -> called := true; f ());
    called
  ;;

  let report_exn t = Server_connection.report_exn t.server_connection

  let shutdown t = Server_connection.shutdown t.server_connection
end

open Runtime

let read ?(eof=false) t str ~off ~len =
  do_read t (fun conn ->
    if eof
    then Server_connection.read_eof conn str ~off ~len
    else Server_connection.read     conn str ~off ~len)
;;

let read_eof = read ~eof:true

let feed_string ?eof t str =
  let len = String.length str in
  let input = Bigstringaf.of_string str ~off:0 ~len in
  read ?eof t input ~off:0 ~len
;;

let read_string ?eof t str =
  let c = feed_string ?eof t str in
  Alcotest.(check int) "read consumes all input" (String.length str) c;
;;

let read_request ?eof t r =
  let request_string = request_to_string r in
  read_string ?eof t request_string
;;

let reader_ready t =
  Alcotest.check read_operation "Reader is ready"
    `Read (current_read_operation t);
;;

let reader_yielded t =
  Alcotest.check read_operation "Reader is in a yield state"
    `Yield (current_read_operation t);
;;

let reader_closed t =
  Alcotest.check read_operation "Reader is closed"
    `Close (current_read_operation t);
;;

(* Checks that the [len] prefixes of expected and the write match, and returns
   the rest. *)
let write_partial_string ?(msg="output written") t expected len =
  do_write t (fun conn bufs ->
    let actual =
      String.sub (Write_operation.iovecs_to_string bufs) 0 len
    in
    Alcotest.(check string) msg (String.sub expected 0 len) actual;
    Server_connection.report_write_result conn (`Ok len);
    String.sub expected len (String.length expected - len));
;;

let write_string ?(msg="output written") t expected =
  do_write t (fun conn bufs ->
    let len = String.length expected in
    let actual = Write_operation.iovecs_to_string bufs in
    Alcotest.(check string) msg expected actual;
    Server_connection.report_write_result conn (`Ok len));
;;


let write_response ?(msg="response written") ?body t r =
  let response_string = response_to_string ?body r in
  write_string ~msg t response_string
;;

let write_eof t =
  do_write t (fun conn _ ->
    Server_connection.report_write_result conn `Closed)
;;

let writer_ready t =
  let is_write =
    Alcotest.testable Write_operation.pp_hum (fun a b ->
      match a, b with
      | `Write _, `Write _ -> true
      | _ -> false)
  in
  Alcotest.check is_write "Writer is ready"
    (`Write []) (current_write_operation t);
;;

let writer_yielded t =
  Alcotest.check write_operation "Writer is in a yield state"
    `Yield (current_write_operation t);
;;

let writer_closed ?(unread = 0) t =
  Alcotest.check write_operation "Writer is closed"
    (`Close unread) (current_write_operation t);
;;

let connection_is_shutdown t =
  reader_closed t;
  writer_closed t;
;;

let raises_writer_closed f =
  (* This is raised when you write to a closed [Faraday.t] *)
  Alcotest.check_raises "raises because writer is closed"
    (Failure "cannot write to closed writer") f
;;

let request_handler_with_body body reqd =
  Body.Reader.close (Reqd.request_body reqd);
  Reqd.respond_with_string reqd (Response.create `OK) body
;;

let default_request_handler reqd =
  request_handler_with_body "" reqd
;;

let echo_handler response reqd =
  let request_body  = Reqd.request_body reqd in
  let response_body = Reqd.respond_with_streaming reqd response in
  let rec on_read buffer ~off ~len =
    Body.Writer.write_string response_body (Bigstringaf.substring ~off ~len buffer);
    Body.Writer.flush response_body (fun () ->
      Body.Reader.schedule_read request_body ~on_eof ~on_read)
  and on_eof () =
    print_endline "echo handler eof";
    Body.Writer.close response_body
  in
  Body.Reader.schedule_read request_body ~on_eof ~on_read;
;;

let streaming_handler ?(flush=false) response writes reqd =
  let writes = ref writes in
  let request_body = Reqd.request_body reqd in
  Body.Reader.close request_body;
  let body = Reqd.respond_with_streaming ~flush_headers_immediately:flush reqd response in
  let rec write () =
    match !writes with
    | [] -> Body.Writer.close body
    | w :: ws ->
      Body.Writer.write_string body w;
      writes := ws;
      Body.Writer.flush body write
  in
  write ();
;;

let synchronous_raise reqd =
  Reqd.report_exn reqd (Failure "caught this exception")
;;

let error_handler ?request:_ _error start_response =
  let resp_body = start_response Headers.empty in
  Body.Writer.write_string resp_body "got an error";
  Body.Writer.close resp_body
;;

let test_initial_reader_state () =
  let t = create default_request_handler in
  Alcotest.check read_operation "A new reader wants input"
    `Read (current_read_operation t);
;;

let test_reader_is_closed_after_eof () =
  let t = create default_request_handler in
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;

  let t = create default_request_handler in
  let c = read t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read with no input returns 0" 0 c;
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0; in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;
;;

let test_single_get () =
  (* Single GET *)
  let t = create default_request_handler in
  read_request   t (Request.create `GET "/");
  write_response t (Response.create `OK);

  (* Single GET, close the connection *)
  let t = create default_request_handler in
  read_request   t (Request.create `GET "/" ~headers:Headers.connection_close);
  write_response t (Response.create `OK);
  connection_is_shutdown t;

  (* Single GET, with reponse body *)
  let response_body = "This is a test" in
  let t = create (request_handler_with_body response_body) in
  read_request   t (Request.create `GET "/" ~headers:Headers.connection_close);
  write_response t
    ~body:response_body
    (Response.create `OK);
  connection_is_shutdown t;
;;

let test_asynchronous_response () =
  let response_body = "hello, world!" in
  let response_body_length = String.length response_body in
  let response =
    Response.create `OK ~headers:(Headers.encoding_fixed response_body_length) in
  let continue = ref (fun () -> ()) in
  let t = create (fun reqd ->
    continue := fun () ->
      Body.Reader.close (Reqd.request_body reqd);
      let data = Bigstringaf.of_string ~off:0 ~len:response_body_length response_body in
      let size = Bigstringaf.length data in
      let response = Response.create `OK ~headers:(Headers.encoding_fixed size) in
      let response_body =
        Reqd.respond_with_streaming reqd response in
      Body.Writer.write_bigstring response_body data;
      Body.Writer.close response_body)
   in
  read_request   t (Request.create `GET "/");
  reader_yielded t;
  writer_yielded t;
  !continue ();
  write_response t ~body:response_body response;
  read_request   t (Request.create `GET "/");
  reader_yielded t;
  writer_yielded t;
  !continue ();
  write_response t ~body:response_body response
;;

let test_echo_post () =
  let request = Request.create `GET "/" ~headers:Headers.encoding_chunked in

  (* Echo a single chunk *)
  let response = Response.create `OK ~headers:Headers.encoding_chunked in
  let t = create (echo_handler response) in
  read_request t request;
  read_string  t "e\r\nThis is a test";
  write_response t
    ~body:"e\r\nThis is a test\r\n"
    response;
  read_string  t "\r\n0\r\n\r\n";
  write_string t "0\r\n\r\n";
  writer_yielded t;

  (* Echo two chunks *)
  let response = Response.create `OK ~headers:Headers.encoding_chunked in
  let t = create (echo_handler response) in
  read_request t request;
  read_string  t "e\r\nThis is a test";
  write_response t
    ~body:"e\r\nThis is a test\r\n"
    response;
  read_string  t "\r\n21\r\n... that involves multiple chunks";
  write_string t "21\r\n... that involves multiple chunks\r\n";
  read_string  t "\r\n0\r\n\r\n";
  write_string t "0\r\n\r\n";
  writer_yielded t;

  (* Echo and close *)
  let response =
    Response.create `OK ~headers:Headers.connection_close
  in
  let t = create (echo_handler response) in
  read_request t request;
  read_string  t "e\r\nThis is a test";
  write_response t
    ~body:"This is a test"
    response;
  read_string  t "\r\n21\r\n... that involves multiple chunks";
  read_string  t "\r\n0\r\n\r\n";
  write_string t "... that involves multiple chunks";
  connection_is_shutdown t;
;;

let test_streaming_response () =
  let request  = Request.create `GET "/" in
  let response = Response.create `OK in

  let t = create (streaming_handler response ["Hello "; "world!"]) in
  read_request   t request;
  write_response t
    ~body:"Hello "
    response;
  write_string   t "world!";
  writer_yielded t;
;;

let test_asynchronous_streaming_response () =
  let request  = Request.create `GET "/" ~headers:Headers.connection_close in
  let response = Response.create `OK in

  let body = ref None in
  let t = create (fun reqd ->
    body := Some (Reqd.respond_with_streaming reqd response))
  in

  writer_yielded t;
  let writer_woken_up =
    on_writer_unyield t (fun () ->
      write_response t ~body:"Hello " response)
  in

  read_request t request;
  let body =
    match !body with
    | None -> failwith "no body found"
    | Some body -> body
  in
  Body.Writer.write_string body "Hello ";
  Alcotest.(check bool) "Writer not woken up"
    false !writer_woken_up;
  Body.Writer.flush body ignore;
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;

  writer_yielded t;
  let writer_woken_up =
    on_writer_unyield t (fun () ->
      write_string t "world!";
      writer_closed t)
  in
  Body.Writer.write_string body "world!";
  Alcotest.(check bool) "Writer not woken up"
    false !writer_woken_up;
  Body.Writer.close body;
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up
;;

let test_asynchronous_streaming_response_with_immediate_flush () =
  let request  = Request.create `GET "/" ~headers:Headers.connection_close in
  let response = Response.create `OK in

  let body = ref None in
  let t = create (fun reqd ->
    body := Some (Reqd.respond_with_streaming reqd response ~flush_headers_immediately:true))
  in
  writer_yielded t;
  let writer_woken_up =
    on_writer_unyield t (fun () ->
      write_response t response);
  in
  Alcotest.(check bool) "Writer not woken up"
    false !writer_woken_up;

  read_request t request;
  let body =
    match !body with
    | None -> failwith "no body found"
    | Some body -> body
  in
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;

  writer_yielded t;
  let writer_woken_up =
    on_writer_unyield t (fun () ->
      writer_closed t)
  in
  Body.Writer.close body;
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up
;;

let test_empty_fixed_streaming_response () =
  let request  = Request.create `GET "/" in
  let response = Response.create `OK ~headers:(Headers.encoding_fixed 0) in

  let t = create (streaming_handler response []) in
  read_request   t request;
  write_response t response;
  writer_yielded t;
;;

let test_empty_chunked_streaming_response () =
  let request  = Request.create `GET "/" in
  let response = Response.create `OK ~headers:Headers.encoding_chunked in

  let t = create (streaming_handler response []) in
  read_request   t request;
  write_response t response
    ~body:"0\r\n\r\n";
  writer_yielded t;
;;

let test_multiple_get () =
  let t = create default_request_handler in
  read_request   t (Request.create `GET "/");
  write_response t (Response.create `OK);
  read_request   t (Request.create `GET "/");
  write_response t (Response.create `OK);
;;

let test_connection_error () =
  let t = create ~error_handler (fun _ -> assert false) in
  let writer_woken_up = on_writer_unyield t ignore in
  report_exn t (Failure "connection failure");
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;
  write_response t
    ~msg:"Error response written"
    (Response.create `Internal_server_error)
    ~body:"got an error"
;;

let test_synchronous_error () =
  let t = create ~error_handler synchronous_raise in
  let writer_woken_up = on_writer_unyield t ignore in
  read_request t (Request.create `GET "/");
  Alcotest.check read_operation "Error shuts down the reader"
    `Close (current_read_operation t);
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;
  (* This shows up in two flushes because [Reqd] creates error reposnses with
     [~flush_headers_immediately:true] *)
  write_response t ~msg:"Error response written"
    (Response.create `Internal_server_error);
  write_string t "got an error";
;;

let test_synchronous_error_asynchronous_handling () =
  let continue = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue := (fun () ->
      error_handler ?request error start_response)
  in
  let t = create ~error_handler synchronous_raise in
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  read_request t (Request.create `GET "/");
  Alcotest.check read_operation "Error shuts down the reader"
    `Close (current_read_operation t);
  Alcotest.(check bool) "Writer not woken up"
    false !writer_woken_up;
  !continue ();
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;
  (* This shows up in two flushes because [Reqd] creates error reposnses with
     [~flush_headers_immediately:true] *)
  write_response t ~msg:"Error response written"
    (Response.create `Internal_server_error);
  write_string t "got an error";
;;


let test_asynchronous_error () =
  let continue = ref (fun () -> ()) in
  let asynchronous_raise reqd =
    continue := (fun () -> synchronous_raise reqd)
  in
  let t = create ~error_handler asynchronous_raise in
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  read_request t (Request.create `GET "/");
  Alcotest.(check bool) "Writer not woken up"
    false !writer_woken_up;
  reader_yielded t;
  !continue ();
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;
  (* This shows up in two flushes because [Reqd] creates error reposnses with
     [~flush_headers_immediately:true] *)
  write_response t ~msg:"Error response written"
    (Response.create `Internal_server_error);
  write_string t "got an error";
  connection_is_shutdown t
;;

let test_asynchronous_error_asynchronous_handling () =
  let continue_request = ref (fun () -> ()) in
  let asynchronous_raise reqd =
    continue_request := (fun () -> synchronous_raise reqd)
  in
  let continue_error = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue_error := (fun () ->
      error_handler ?request error start_response)
  in
  let t = create ~error_handler asynchronous_raise in
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  read_request t (Request.create `GET "/");
  Alcotest.(check bool) "Writer not woken up"
    false !writer_woken_up;
  reader_yielded t;
  !continue_request ();
  Alcotest.(check bool) "Writer not woken up"
    false !writer_woken_up;
  !continue_error ();
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;
  (* This shows up in two flushes because [Reqd] creates error reposnses with
     [~flush_headers_immediately:true] *)
  write_response t ~msg:"Error response written"
    (Response.create `Internal_server_error);
  write_string t "got an error";
  connection_is_shutdown t
;;

let test_error_while_parsing () =
  let continue_error = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue_error := (fun () ->
      error_handler ?request error start_response)
  in
  let setup () =
    let t = create ~error_handler (fun _ -> assert false) in
    let n = feed_string t "GET / HTTP/1.1\r\n" in
    Alcotest.(check int) "read bytes" 16 n;
    reader_ready t;
    report_exn t (Failure "runtime error during parse");
    t
  in

  (* Handle before read *)
  let t = setup () in
  !continue_error ();
  write_response t ~msg:"Error response written"
    (Response.create `Internal_server_error)
    ~body:"got an error";
  writer_closed t;
  (* XXX(dpatti): Runtime is in a read loop and must report something. I don't
     know if this could ever deadlock or if that's a runtime concern. *)
  reader_ready t;
  let n = feed_string t "Host: localhost\r\n" in
  Alcotest.(check int) "read bytes" 0 n;
  reader_closed t;

  (* Read before handle *)
  let t = setup () in
  reader_ready t;
  let n = feed_string t "Host: localhost\r\n" in
  Alcotest.(check int) "read bytes" 0 n;
  reader_closed t;
  !continue_error ();
  write_response t ~msg:"Error response written"
    (Response.create `Internal_server_error)
    ~body:"got an error";
  writer_closed t;
;;

let test_error_before_read () =
  let request_handler _ = assert false in
  let invoked_error_handler = ref false in
  let error_handler ?request:_ _ _ =
    invoked_error_handler := true;
  in
  let t = create ~error_handler request_handler in
  report_exn t (Failure "immediate runtime error");
  reader_ready t;
  writer_yielded t;
  (* XXX(dpatti): This seems wrong to me. Should we be sending responses when we
     haven't even read any bytes yet? Maybe too much of an edge case to worry. *)
  Alcotest.(check bool) "Error handler was invoked" true !invoked_error_handler;
;;

let test_error_left_unhandled () =
  let error_handler ?request:_ _ _ = () in
  let t = create ~error_handler (fun _ -> ()) in
  read_request t (Request.create `GET "/");
  report_exn t (Failure "runtime error");
  (* If the error handler is invoked and does not try to complete a response,
     the connection will hang. This is not necessarily desirable but rather a
     tradeoff to let the user respond asynchronously. *)
  reader_yielded t;
  writer_yielded t;
;;

let test_chunked_encoding () =
  let request_handler reqd =
    let response = Response.create `OK ~headers:Headers.encoding_chunked in
    let resp_body = Reqd.respond_with_streaming reqd response in
    Body.Writer.write_string resp_body "First chunk";
    Body.Writer.flush resp_body (fun () ->
      Body.Writer.write_string resp_body "Second chunk";
      Body.Writer.close resp_body);
  in
  let t = create ~error_handler request_handler in
  writer_yielded t;
  read_request t (Request.create `GET "/");
  write_response t
    ~msg:"First chunk written"
    ~body:"b\r\nFirst chunk\r\n"
    (Response.create `OK ~headers:Headers.encoding_chunked);
  write_string t
    ~msg:"Second chunk"
    "c\r\nSecond chunk\r\n";
  write_string t
    ~msg:"Final chunk written"
    "0\r\n\r\n";
  Alcotest.check read_operation "Keep-alive"
    `Read (current_read_operation t);
;;

let test_chunked_encoding_for_error () =
  let error_handler ?request error start_response =
    Alcotest.(check (option request)) "No parsed request"
      None request;
    Alcotest.(check request_error) "Request error"
      `Bad_request error;
    let body = start_response Headers.encoding_chunked in
    Body.Writer.write_string body "Bad";
    Body.Writer.flush body (fun () ->
      Body.Writer.write_string body " request";
      Body.Writer.close body);
  in
  let t = create ~error_handler (fun _ -> assert false) in
  let c = feed_string t "  X\r\n\r\n" in
  Alcotest.(check int) "Partial read" 2 c;
  (* XXX(dpatti): Note that even if we use a chunked encoding header, we still
     write it without any encoding *)
  write_response t
    (Response.create `Bad_request ~headers:Headers.encoding_chunked)
    ~body:"Bad";
  write_string t " request";
  connection_is_shutdown t;
;;

let test_blocked_write_on_chunked_encoding () =
  let request_handler reqd =
    let response = Response.create `OK ~headers:Headers.encoding_chunked in
    let resp_body = Reqd.respond_with_streaming reqd response in
    Body.Writer.write_string resp_body "gets partially written";
    Body.Writer.flush resp_body ignore;
    (* Response body never gets closed but for the purposes of the test, that's
     * OK. *)
  in
  let t = create ~error_handler request_handler in
  writer_yielded t;
  read_request t (Request.create `GET "/");
  let response_bytes =
    "HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n16\r\ngets partially written\r\n"
  in
  let second_write =
    write_partial_string t ~msg:"first write" response_bytes 16
  in
  write_string t ~msg:"second write" second_write
;;

let test_unexpected_eof () =
  let t = create default_request_handler in
  read_request   t (Request.create `GET "/");
  write_eof      t;
  writer_closed  t ~unread:19;
;;

let test_input_shrunk () =
  let continue_response = ref (fun () -> ()) in
  let error_handler ?request:_ _ = assert false in
  let request_handler reqd =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      [ "Host"           , "example.com"
      ; "Connection"     , "close"
      ; "Accept"         , "application/json, text/plain, */*"
      ; "Accept-Language", "en-US,en;q=0.5" ]
      (Headers.to_list (Reqd.request reqd).headers);
    Body.Reader.close (Reqd.request_body reqd);
    continue_response := (fun () ->
      Reqd.respond_with_string reqd (Response.create `OK) "");
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  let writer_woken_up =
    on_writer_unyield t (fun () ->
      write_response t (Response.create `OK))
  in
  let len = feed_string t "GET /v1/b HTTP/1.1\r\nH" in
  Alcotest.(check int) "partial read" 20 len;
  read_string t "Host: example.com\r\n\
Connection: close\r\n\
Accept: application/json, text/plain, */*\r\n\
Accept-Language: en-US,en;q=0.5\r\n\r\n";
  Alcotest.(check bool) "Writer not woken up"
    false !writer_woken_up;
  reader_closed t;
  !continue_response ();
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;
  writer_closed t;
;;

let test_failed_request_parse () =
  let error_handler_fired = ref false in
  let error_handler ?request error start_response =
    error_handler_fired := true;
    Alcotest.(check (option request)) "No parsed request"
      None request;
    Alcotest.(check request_error) "Request error"
      `Bad_request error;
    start_response Headers.empty |> Body.Writer.close;
  in
  let request_handler _reqd = assert false in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  let len = feed_string t "GET /v1/b HTTP/1.1\r\nHost : example.com\r\n\r\n" in
  (* Reads through the end of "Host" *)
  Alcotest.(check int) "partial read" 24 len;
  reader_closed t;
  Alcotest.(check bool) "Error handler fired"
    true !error_handler_fired;
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;
  write_response t (Response.create `Bad_request);
;;

let test_bad_request () =
  (* A `Bad_request is returned in a number of cases surrounding
     transfer-encoding or content-length headers. *)
  let request = Request.create `GET "/" ~headers:(Headers.encoding_fixed (-1)) in
  let error_handler_fired = ref false in
  let error_handler ?request:request' error start_response =
    error_handler_fired := true;
    Alcotest.(check (option request)) "Parsed request"
      (Some request) request';
    Alcotest.(check request_error) "Request error"
      `Bad_request error;
    start_response Headers.empty |> Body.Writer.close;
  in
  let request_handler _reqd = assert false in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  let writer_woken_up = on_writer_unyield t ignore in
  read_request t request;
  reader_closed t;
  Alcotest.(check bool) "Error handler fired"
    true !error_handler_fired;
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;
  write_response t (Response.create `Bad_request);
;;

let test_multiple_requests_in_single_read () =
  let response = Response.create `OK in
  let t =
    create (fun reqd ->
      Reqd.respond_with_string reqd response "")
  in
  let reqs =
    request_to_string (Request.create `GET "/") ^
    request_to_string (Request.create `GET "/")
  in
  read_string t reqs;
  write_response t response;
  write_response t response;
;;

let test_multiple_async_requests_in_single_read () =
  let response = Response.create `OK in
  let reqs_handled = ref 0 in
  let finish_handler = ref (fun () -> assert false) in
  let t =
    create (fun reqd ->
      reqs_handled := !reqs_handled + 1;
      finish_handler := (fun () ->
        Reqd.respond_with_string reqd response ""))
  in
  let reqs =
    request_to_string (Request.create `GET "/") ^
    request_to_string (Request.create `GET "/")
  in
  read_string t reqs;
  reader_yielded t;
  writer_yielded t;
  Alcotest.(check int) "fired handler once" 1 !reqs_handled;
  !finish_handler ();
  write_response t response;
  Alcotest.(check int) "fired handler again" 2 !reqs_handled;
  !finish_handler ();
  write_response t response;
  reader_ready t;
;;

let test_multiple_requests_in_single_read_with_close () =
  let response = Response.create `OK ~headers:Headers.connection_close in
  let t =
    create (fun reqd -> Reqd.respond_with_string reqd response "")
  in
  let reqs =
    request_to_string (Request.create `GET "/") ^
    request_to_string (Request.create `GET "/")
  in
  read_string t reqs;
  write_response t response;
  connection_is_shutdown t;
;;

let test_multiple_requests_in_single_read_with_eof () =
  let response = Response.create `OK in
  let t =
    create (fun reqd -> Reqd.respond_with_string reqd response "")
  in
  let reqs =
    request_to_string (Request.create `GET "/") ^
    request_to_string (Request.create `GET "/")
  in
  read_string t reqs ~eof:true;
  write_response t response;
  write_response t response;
  connection_is_shutdown t;
;;

let test_parse_failure_after_checkpoint () =
  let error_queue = ref None in
  let error_handler ?request:_ error _start_response =
    Alcotest.(check (option reject)) "Error queue is empty" !error_queue None;
    error_queue := Some error
  in
  let request_handler _reqd = assert false in
  let t = create ~error_handler request_handler in
  reader_ready t;
  read_string t "GET index.html HTTP/1.1\r\n";
  let result = feed_string t " index.html HTTP/1.1\r\n\r\n" in
  Alcotest.(check int) "Bad header not consumed" result 0;
  reader_closed t;
  match !error_queue with
  | None -> Alcotest.fail "Expected error"
  | Some error -> Alcotest.(check request_error) "Error" error `Bad_request
;;

let test_parse_failure_at_eof () =
  let error_queue = ref None in
  let continue = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    Alcotest.(check (option reject)) "Error queue is empty" !error_queue None;
    Alcotest.(check (option reject)) "Request was not parsed" request None;
    error_queue := Some error;
    continue := (fun () ->
      let resp_body = start_response Headers.empty in
      Body.Writer.write_string resp_body "got an error";
      Body.Writer.close resp_body);
  in
  let request_handler _reqd = assert false in
  let t = create ~error_handler request_handler in
  reader_ready t;
  read_string t "GET index.html HTTP/1.1\r\n";
  let result = feed_string ~eof:true t " index.html HTTP/1.1\r\n\r\n" in
  Alcotest.(check int) "Bad header not consumed" result 0;
  reader_closed t;
  (match !error_queue with
   | None -> Alcotest.fail "Expected error"
   | Some error -> Alcotest.(check request_error) "Error" error `Bad_request);
  !continue ();
  write_response t (Response.create `Bad_request) ~body:"got an error";
  writer_closed t;
;;

let test_response_finished_before_body_read () =
  let response = Response.create `OK ~headers:(Headers.encoding_fixed 4) in
  let rev_body_chunks = ref [] in
  let request_handler reqd =
    let rec read_body () =
      Body.Reader.schedule_read
        (Reqd.request_body reqd)
        ~on_read:(fun buf ~off ~len ->
          rev_body_chunks := Bigstringaf.substring buf ~off ~len :: !rev_body_chunks;
          read_body ())
        ~on_eof:ignore;
    in
    read_body ();
    Reqd.respond_with_string reqd response "done"
  in
  let t = create request_handler in
  read_request t (Request.create `GET "/" ~headers:(Headers.encoding_fixed 12));
  write_response t response ~body:"done";
  (* Finish the request and send another *)
  read_string t "hello, ";
  read_string t "world";
  Alcotest.(check (list string)) "received body" ["world"; "hello, "] !rev_body_chunks;
  read_request t (Request.create `GET "/");
  write_response t response ~body:"done";
;;

let test_shutdown_in_request_handler () =
  let request = Request.create `GET "/" in
  let rec t =
    lazy (create (fun _ -> shutdown (Lazy.force t)))
  in
  let t = Lazy.force t in
  read_request t request;
  reader_closed t;
  writer_closed t
;;

let test_shutdown_during_asynchronous_request () =
  let request = Request.create `GET "/" in
  let response = Response.create `OK in
  let continue = ref (fun () -> ()) in
  let t = create (fun reqd ->
    continue := (fun () ->
      Reqd.respond_with_string reqd response ""))
  in
  read_request t request;
  shutdown t;
  raises_writer_closed !continue;
  reader_closed t;
  writer_closed t
;;

let test_flush_response_before_shutdown () =
  let request = Request.create `GET "/" ~headers:(Headers.encoding_fixed 0) in
  let response = Response.create `OK ~headers:Headers.encoding_chunked in
  let continue = ref (fun () -> ()) in
  let request_handler reqd =
    let body = Reqd.respond_with_streaming ~flush_headers_immediately:true reqd response in
    continue := (fun () ->
      Body.Writer.write_string body "hello world";
      Body.Writer.close body);
  in
  let t = create request_handler in
  read_request t request;
  write_response t response;
  !continue ();
  shutdown t;
  raises_writer_closed (fun () ->
    write_string t "b\r\nhello world\r\n";
    connection_is_shutdown t);
;;

let test_schedule_read_with_data_available () =
  let response = Response.create `OK in
  let body = ref None in
  let continue = ref (fun () -> ()) in
  let request_handler reqd =
    body := Some (Reqd.request_body reqd);
    continue := (fun () ->
      Reqd.respond_with_string reqd response "")
  in
  let t = create request_handler in
  read_request t (Request.create `GET "/" ~headers:(Headers.encoding_fixed 6));

  let body = Option.get !body in
  let schedule_read expected =
    let did_read = ref false in
    Body.Reader.schedule_read body
      ~on_read:(fun buf ~off ~len ->
        let actual = Bigstringaf.substring buf ~off ~len in
        did_read := true;
        Alcotest.(check string) "Body" expected actual)
      ~on_eof:(fun () -> assert false);
    Alcotest.(check bool) "on_read called" true !did_read;
  in

  (* We get some data on the connection, but not the full response yet. *)
  read_string t "Hello";
  (* Schedule a read when there is already data available. on_read should be
     called synchronously *)
  schedule_read "Hello";
  read_string t "!";
  schedule_read "!";
  (* Also works with eof *)
  Body.Reader.schedule_read body
    ~on_read:(fun _ ~off:_ ~len:_ -> Alcotest.fail "Expected eof")
    ~on_eof:(fun () -> !continue ());
  write_response t response;
;;

let tests =
  [ "initial reader state"  , `Quick, test_initial_reader_state
  ; "shutdown reader closed", `Quick, test_reader_is_closed_after_eof
  ; "single GET"            , `Quick, test_single_get
  ; "multiple GETs"         , `Quick, test_multiple_get
  ; "asynchronous response" , `Quick, test_asynchronous_response
  ; "echo POST"             , `Quick, test_echo_post
  ; "streaming response"    , `Quick, test_streaming_response
  ; "asynchronous streaming response", `Quick, test_asynchronous_streaming_response
  ; "asynchronous streaming response, immediate flush", `Quick, test_asynchronous_streaming_response_with_immediate_flush
  ; "empty fixed streaming response", `Quick, test_empty_fixed_streaming_response
  ; "empty chunked streaming response", `Quick, test_empty_chunked_streaming_response
  ; "connection error", `Quick, test_connection_error
  ; "synchronous error, synchronous handling", `Quick, test_synchronous_error
  ; "synchronous error, asynchronous handling", `Quick, test_synchronous_error_asynchronous_handling
  ; "asynchronous error, synchronous handling", `Quick, test_asynchronous_error
  ; "asynchronous error, asynchronous handling", `Quick, test_asynchronous_error_asynchronous_handling
  ; "error while parsing", `Quick, test_error_while_parsing
  ; "error before read", `Quick, test_error_before_read
  ; "error left unhandled", `Quick, test_error_left_unhandled
  ; "chunked encoding", `Quick, test_chunked_encoding
  ; "chunked encoding for error", `Quick, test_chunked_encoding_for_error
  ; "blocked write on chunked encoding", `Quick, test_blocked_write_on_chunked_encoding
  ; "writer unexpected eof", `Quick, test_unexpected_eof
  ; "input shrunk", `Quick, test_input_shrunk
  ; "failed request parse", `Quick, test_failed_request_parse
  ; "bad request", `Quick, test_bad_request
  ; "multiple requests in single read", `Quick, test_multiple_requests_in_single_read
  ; "multiple async requests in single read", `Quick, test_multiple_async_requests_in_single_read
  ; "multiple requests with connection close", `Quick, test_multiple_requests_in_single_read_with_close
  ; "multiple requests with eof", `Quick, test_multiple_requests_in_single_read_with_eof
  ; "parse failure after checkpoint", `Quick, test_parse_failure_after_checkpoint
  ; "parse failure at eof", `Quick, test_parse_failure_at_eof
  ; "response finished before body read", `Quick, test_response_finished_before_body_read
  ; "shutdown in request handler", `Quick, test_shutdown_in_request_handler
  ; "shutdown during asynchronous request", `Quick, test_shutdown_during_asynchronous_request
  ; "flush response before shutdown", `Quick, test_flush_response_before_shutdown
  ; "schedule read with data available", `Quick, test_schedule_read_with_data_available
  ]
