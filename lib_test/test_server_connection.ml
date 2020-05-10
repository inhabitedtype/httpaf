open Httpaf
open Helpers
open Server_connection

let feed_string t str =
  let len = String.length str in
  let input = Bigstringaf.of_string str ~off:0 ~len in
  read t input ~off:0 ~len

let read_string t str =
  let c = feed_string t str in
  Alcotest.(check int) "read consumes all input" (String.length str) c;
;;

let read_request t r =
  let request_string = request_to_string r in
  read_string t request_string
;;

let reader_ready t =
  Alcotest.check read_operation "Reader is ready"
    `Read (next_read_operation t);
;;

let reader_yielded t =
  Alcotest.check read_operation "Reader is in a yield state"
    `Yield (next_read_operation t);
;;

let write_string ?(msg="output written") t str =
  let len = String.length str in
  Alcotest.(check (option string)) msg
    (Some str)
    (next_write_operation t |> Write_operation.to_write_as_string);
  report_write_result t (`Ok len);
;;

let write_response ?(msg="response written") ?body t r =
  let response_string = response_to_string ?body r in
  write_string ~msg t response_string
;;

let write_eof t =
  report_write_result t `Closed;
;;

let writer_yielded t =
  Alcotest.check write_operation "Writer is in a yield state"
    `Yield (next_write_operation t);
;;

let writer_closed ?(unread = 0) t =
  Alcotest.check write_operation "Writer is closed"
    (`Close unread) (next_write_operation t);
;;

let connection_is_shutdown t =
  Alcotest.check read_operation "Reader is closed"
    `Close (next_read_operation t);
  writer_closed  t;
;;

let request_handler_with_body body reqd =
  Body.close_reader (Reqd.request_body reqd);
  Reqd.respond_with_string reqd (Response.create `OK) body
;;

let default_request_handler reqd =
  request_handler_with_body "" reqd
;;

let echo_handler response reqd =
  let request_body  = Reqd.request_body reqd in
  let response_body = Reqd.respond_with_streaming reqd response in
  let rec on_read buffer ~off ~len =
    Body.write_string response_body (Bigstringaf.substring ~off ~len buffer);
    Body.flush response_body (fun () ->
      Body.schedule_read request_body ~on_eof ~on_read)
    and on_eof () = print_endline "got eof"; Body.close_writer response_body in
  Body.schedule_read request_body ~on_eof ~on_read;
;;

let streaming_handler ?(flush=false) response writes reqd =
  let writes = ref writes in
  let request_body = Reqd.request_body reqd in
  Body.close_reader request_body;
  let body = Reqd.respond_with_streaming ~flush_headers_immediately:flush reqd response in
  let rec write () =
    match !writes with
    | [] -> Body.close_writer body
    | w :: ws ->
      Body.write_string body w;
      writes := ws;
      Body.flush body write
  in
  write ();
;;

let synchronous_raise reqd =
  Reqd.report_exn reqd (Failure "caught this exception")
;;

let error_handler ?request:_ _error start_response =
  let resp_body = start_response Headers.empty in
  Body.write_string resp_body "got an error";
  Body.close_writer resp_body
;;

let test_initial_reader_state () =
  let t = create default_request_handler in
  Alcotest.check read_operation "A new reader wants input"
    `Read (next_read_operation t);
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
  read_request   t (Request.create `GET "/" ~headers:(Headers.of_list ["connection", "close"]));
  write_response t (Response.create `OK);
  connection_is_shutdown t;

  (* Single GET, with reponse body *)
  let response_body = "This is a test" in
  let t = create (request_handler_with_body response_body) in
  read_request   t (Request.create `GET "/" ~headers:(Headers.of_list ["connection", "close"]));
  write_response t
    ~body:response_body
    (Response.create `OK);
  connection_is_shutdown t;
;;

let test_asynchronous_response () =
  let response_body = "hello, world!" in
  let response_body_length = String.length response_body in
  let response =
    Response.create
      `OK
      ~headers:(Headers.of_list [("content-length", string_of_int response_body_length)])
  in
  let continue = ref (fun () -> ()) in
  let t = create (fun reqd ->
    continue := fun () ->
      Body.close_reader (Reqd.request_body reqd);
      let data = Bigstringaf.of_string ~off:0 ~len:response_body_length response_body in
      let size = Bigstringaf.length data in
      let response =
        Response.create
          `OK
          ~headers:(Headers.of_list [("content-length", string_of_int size)])
      in
      let response_body =
        Reqd.respond_with_streaming reqd response in
      Body.write_bigstring response_body data;
      Body.close_writer response_body)
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
  let request = Request.create `GET "/" ~headers:(Headers.of_list ["transfer-encoding", "chunked"]) in

  (* Echo a single chunk *)
  let response =
    Response.create `OK ~headers:(Headers.of_list ["transfer-encoding", "chunked"])
  in
  let t = create (echo_handler response) in
  read_request t request;
  read_string  t "e\r\nThis is a test";
  write_response t
    ~body:"e\r\nThis is a test\r\n"
    response;
  read_string  t "\r\n0\r\n";
  write_string t "0\r\n\r\n";
  writer_yielded t;

  (* Echo two chunks *)
  let response =
    Response.create `OK ~headers:(Headers.of_list ["transfer-encoding", "chunked"])
  in
  let t = create (echo_handler response) in
  read_request t request;
  read_string  t "e\r\nThis is a test";
  write_response t
    ~body:"e\r\nThis is a test\r\n"
    response;
  read_string  t "\r\n21\r\n... that involves multiple chunks";
  write_string t "21\r\n... that involves multiple chunks\r\n";
  read_string  t "\r\n0\r\n";
  write_string t "0\r\n\r\n";
  writer_yielded t;

  (* Echo and close *)
  let response =
    Response.create `OK ~headers:(Headers.of_list ["connection", "close"])
  in
  let t = create (echo_handler response) in
  read_request t request;
  read_string  t "e\r\nThis is a test";
  write_response t
    ~body:"This is a test"
    response;
  read_string  t "\r\n21\r\n... that involves multiple chunks";
  read_string  t "\r\n0\r\n";
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
  let request  = Request.create `GET "/" ~headers:(Headers.of_list ["connection", "close"]) in
  let response = Response.create `OK in

  let body = ref None in
  let t = create (fun reqd ->
    body := Some (Reqd.respond_with_streaming reqd response))
  in

  let writer_woken_up = ref false in
  writer_yielded t;
  yield_writer t (fun () ->
    writer_woken_up := true;
    writer_yielded t);
  Alcotest.(check bool) "Writer not woken up"
    false !writer_woken_up;

  read_request t request;
  let body =
    match !body with
    | None -> failwith "no body found"
    | Some body -> body
  in
  (* XXX(dpatti): This is an observation of a current behavior where the writer
     is awoken only to find that it was asked to yield again. It is cleaned up
     in another branch where we move the continuation off of the reqd/body. *)
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;
  let writer_woken_up = ref false in
  yield_writer t (fun () ->
    writer_woken_up := true;
    write_response t ~body:"Hello " response);

  Body.write_string body "Hello ";
  Alcotest.(check bool) "Writer not woken up"
    false !writer_woken_up;
  Body.flush body ignore;
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;

  let writer_woken_up = ref false in
  writer_yielded t;
  yield_writer t (fun () ->
    writer_woken_up := true;
    write_string t "world!";
    writer_closed t);
  Body.write_string body "world!";
  Alcotest.(check bool) "Writer not woken up"
    false !writer_woken_up;
  Body.close_writer body;
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up
;;

let test_asynchronous_streaming_response_with_immediate_flush () =
  let request  = Request.create `GET "/" ~headers:(Headers.of_list ["connection", "close"]) in
  let response = Response.create `OK in

  let body = ref None in
  let t = create (fun reqd ->
    body := Some (Reqd.respond_with_streaming reqd response ~flush_headers_immediately:true))
  in
  let writer_woken_up = ref false in
  writer_yielded t;
  yield_writer t (fun () ->
    writer_woken_up := true;
    write_response t response);
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

  let writer_woken_up = ref false in
  writer_yielded t;
  yield_writer t (fun () ->
    writer_woken_up := true;
    writer_closed t);
  Body.close_writer body;
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up
;;

let test_empty_fixed_streaming_response () =
  let request  = Request.create `GET "/" in
  let response =
    Response.create `OK
      ~headers:(Headers.of_list ["Content-length", "0"])
  in

  let t = create (streaming_handler response []) in
  read_request   t request;
  write_response t response;
  writer_yielded t;
;;

let test_empty_chunked_streaming_response () =
  let request  = Request.create `GET "/" in
  let response =
    Response.create `OK
      ~headers:(Headers.of_list ["Transfer-encoding", "chunked"])
  in

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

let test_synchronous_error () =
  let writer_woken_up = ref false in
  let t = create ~error_handler synchronous_raise in
  yield_writer t (fun () -> writer_woken_up := true);
  read_request t (Request.create `GET "/");
  Alcotest.check read_operation "Error shuts down the reader"
    `Close (next_read_operation t);
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;
  write_response t
    ~msg:"Error response written"
    (Response.create `Internal_server_error)
    ~body:"got an error"
;;

let test_synchronous_error_asynchronous_handling () =
  let writer_woken_up = ref false in
  let continue = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue := (fun () ->
      error_handler ?request error start_response)
  in
  let t = create ~error_handler synchronous_raise in
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  read_request t (Request.create `GET "/");
  Alcotest.check read_operation "Error shuts down the reader"
    `Close (next_read_operation t);
  writer_yielded t;
  !continue ();
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;
  write_response t
    ~msg:"Error response written"
    (Response.create `Internal_server_error)
    ~body:"got an error"
;;


let test_asynchronous_error () =
  let continue = ref (fun () -> ()) in
  let asynchronous_raise reqd =
    continue := (fun () -> synchronous_raise reqd)
  in
  let writer_woken_up = ref false in
  let t = create ~error_handler asynchronous_raise in
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  read_request t (Request.create `GET "/");
  writer_yielded t;
  reader_yielded t;
  !continue ();
  Alcotest.check read_operation "Error shuts down the reader"
    `Close (next_read_operation t);
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;
  write_response t
    ~msg:"Error response written"
    (Response.create `Internal_server_error)
    ~body:"got an error"
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
  let writer_woken_up = ref false in
  let t = create ~error_handler asynchronous_raise in
  writer_yielded t;
  yield_writer   t (fun () -> writer_woken_up := true);
  read_request   t (Request.create `GET "/");
  writer_yielded t;
  reader_yielded t;
  !continue_request ();
  writer_yielded t;
  !continue_error ();
  Alcotest.check read_operation "Error shuts down the reader"
    `Close (next_read_operation t);
  Alcotest.(check bool) "Writer woken up"
    true !writer_woken_up;
  write_response t
    ~msg:"Error response written"
    (Response.create `Internal_server_error)
    ~body:"got an error"
;;

let test_chunked_encoding () =
  let request_handler reqd =
    let response =
      Response.create `OK
        ~headers:(Headers.of_list [ "Transfer-encoding", "chunked" ])
    in
    let resp_body = Reqd.respond_with_streaming reqd response in
    Body.write_string resp_body "First chunk";
    Body.flush resp_body (fun () ->
      Body.write_string resp_body "Second chunk";
      Body.close_writer resp_body);
  in
  let t = create ~error_handler request_handler in
  writer_yielded t;
  read_request t (Request.create `GET "/");
  write_response t
    ~msg:"First chunk written"
    ~body:"b\r\nFirst chunk\r\n"
    (Response.create `OK ~headers:(Headers.of_list ["Transfer-encoding", "chunked"]));
  write_string t
    ~msg:"Second chunk"
    "c\r\nSecond chunk\r\n";
  write_string t
    ~msg:"Final chunk written"
    "0\r\n\r\n";
  Alcotest.check read_operation "Keep-alive"
    `Read (next_read_operation t);
;;

let test_blocked_write_on_chunked_encoding () =
  let request_handler reqd =
    let response =
      Response.create `OK
        ~headers:(Headers.of_list [ "Transfer-encoding", "chunked" ])
    in
    let resp_body = Reqd.respond_with_streaming reqd response in
    Body.write_string resp_body "gets partially written";
    (* Response body never gets closed but for the purposes of the test, that's
     * OK. *)
  in
  let t = create ~error_handler request_handler in
  writer_yielded t;
  read_request t (Request.create `GET "/");
  let first_write = "HTTP/1.1 200 OK\r\nTransfer-encoding: chunked\r\n\r\n16\r\ngets partially written\r\n" in
  Alcotest.(check (option string)) "first write"
    (Some first_write)
    (next_write_operation t |> Write_operation.to_write_as_string);
  report_write_result t (`Ok 16);
  Alcotest.(check (option string)) "second write"
    (Some (String.sub first_write 16 (String.length first_write - 16)))
    (next_write_operation t |> Write_operation.to_write_as_string);
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
      (Headers.to_rev_list (Reqd.request reqd).headers);
    Body.close_reader (Reqd.request_body reqd);
    continue_response := (fun () ->
      Reqd.respond_with_string reqd (Response.create `OK) "");
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  yield_writer t (fun () ->
    write_response t (Response.create `OK);
  );
  let len = feed_string t "GET /v1/b HTTP/1.1\r\nH" in
  Alcotest.(check int) "partial read" 20 len;
  read_string t "Host: example.com\r\n\
Connection: close\r\n\
Accept: application/json, text/plain, */*\r\n\
Accept-Language: en-US,en;q=0.5\r\n\r\n";
  writer_yielded t;
  Alcotest.check read_operation "reader closed"
    `Close (next_read_operation t);
  !continue_response ();
  writer_closed t;
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
  ; "synchronous error, synchronous handling", `Quick, test_synchronous_error
  ; "synchronous error, asynchronous handling", `Quick, test_synchronous_error_asynchronous_handling
  ; "asynchronous error, synchronous handling", `Quick, test_asynchronous_error
  ; "asynchronous error, asynchronous handling", `Quick, test_asynchronous_error_asynchronous_handling
  ; "chunked encoding", `Quick, test_chunked_encoding
  ; "blocked write on chunked encoding", `Quick, test_blocked_write_on_chunked_encoding
  ; "writer unexpected eof", `Quick, test_unexpected_eof
  ; "input shrunk", `Quick, test_input_shrunk
  ]
