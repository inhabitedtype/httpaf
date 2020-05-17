open Httpaf
open Helpers
open Client_connection

let response_error_pp_hum fmt = function
  | `Malformed_response str ->
    Format.fprintf fmt "Malformed_response: %s" str
  | `Invalid_response_body_length resp ->
    Format.fprintf fmt "Invalid_response_body_length: %s" (response_to_string resp)
  | `Exn exn ->
    Format.fprintf fmt "Exn (%s)" (Printexc.to_string exn)
;;

module Response = struct
  include Response

  let pp = pp_hum
  let equal x y = x = y
end

module Alcotest = struct
  include Alcotest

  let response_error = of_pp response_error_pp_hum
end

let feed_string t str =
  let len = String.length str in
  let input = Bigstringaf.of_string str ~off:0 ~len in
  read t input ~off:0 ~len

let read_string t str =
  let c = feed_string t str in
  Alcotest.(check int) "read consumes all input" (String.length str) c;
;;

let read_response t r =
  let response_string = response_to_string r in
  read_string t response_string
;;

let reader_ready t =
  Alcotest.check read_operation "Reader is ready"
    `Read (next_read_operation t :> [`Close | `Read | `Yield]);
;;

let write_string ?(msg="output written") t str =
  let len = String.length str in
  Alcotest.(check (option string)) msg
    (Some str)
    (next_write_operation t |> Write_operation.to_write_as_string);
  report_write_result t (`Ok len);
;;

let write_request ?(msg="request written") t r =
  let request_string = request_to_string r in
  write_string ~msg t request_string
;;

let writer_yielded t =
  Alcotest.check write_operation "Writer is in a yield state"
    `Yield (next_write_operation t);
;;

let writer_closed t =
  Alcotest.check write_operation "Writer is closed"
    (`Close 0) (next_write_operation t);
;;

let connection_is_shutdown t =
  Alcotest.check read_operation "Reader is closed"
    `Close (next_read_operation t :> [`Close | `Read | `Yield]);
  writer_closed t;
;;

let default_response_handler expected_response response body =
  Alcotest.check (module Response) "expected response" expected_response response;
  let on_read _ ~off:_ ~len:_ = () in
  let on_eof () = () in
  Body.schedule_read body ~on_read ~on_eof;
;;

let no_error_handler _ = assert false

let test_get () =
  let request' = Request.create `GET "/" in
  let response = Response.create `OK in

  (* Single GET *)
  let body, t =
    request
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body;
  write_request  t request';
  writer_closed  t;
  read_response  t response;

  (* Single GET, response closes connection *)
  let response =
    Response.create `OK ~headers:(Headers.of_list [ "connection", "close" ])
  in
  let body, t =
    request
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body;
  write_request  t request';
  read_response  t response;
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;

  (* Single GET, streaming body *)
  let response =
    Response.create `OK ~headers:(Headers.of_list [ "transfer-encoding", "chunked" ])
  in
  let body, t =
    request
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body;
  write_request  t request';
  read_response  t response;
  read_string    t "d\r\nHello, world!\r\n0\r\n\r\n";
;;

let test_response_eof () =
  let request' = Request.create `GET "/" in
  let response = Response.create `OK in (* not actually writen to the channel *)

  let error_message = ref None in
  let body, t =
    request
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:(function
        | `Malformed_response msg -> error_message := Some msg
        | _ -> assert false)
  in
  Body.close_writer body;
  write_request  t request';
  writer_closed  t;
  reader_ready t;
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;
  Alcotest.(check (option string)) "unexpected eof"
    (Some "unexpected eof")
    !error_message
;;

let test_response_header_order () =
  let request' = Request.create `GET "/" in
  let headers =
    [ "a", "1"
    ; "b", "2"
    ; "c", "3"
    ]
  in
  let response = Response.create `OK ~headers:(Headers.of_list headers) in
  let received = ref None in
  let body, t =
    request
      request'
      ~response_handler:(fun response _ -> received := Some response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body;
  write_request t request';
  writer_closed t;
  read_response t response;
  match !received with
  | None -> assert false
  | Some received ->
    Alcotest.(check (list (pair string string))) "headers are equal"
      headers (Headers.to_list received.headers);
;;

let test_report_exn () =
  let request' = Request.create `GET "/" in
  let response = Response.create `OK in (* not actually writen to the channel *)

  let error_message = ref None in
  let body, t =
    request
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:(function
        | `Exn (Failure msg) -> error_message := Some msg
        | _ -> assert false)
  in
  Body.close_writer body;
  write_request  t request';
  writer_closed  t;
  reader_ready t;
  report_exn t (Failure "something went wrong");
  connection_is_shutdown t;
  Alcotest.(check (option string)) "something went wrong"
    (Some "something went wrong")
    !error_message
;;

let test_input_shrunk () =
  let request' = Request.create `GET "/" in
  let response = Response.create `OK in (* not actually writen to the channel *)

  let error_message = ref None in
  let body, t =
    request
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:(function
        | `Exn (Failure msg) -> error_message := Some msg
        | _ -> assert false)
  in
  Body.close_writer body;
  write_request  t request';
  writer_closed  t;
  reader_ready t;
  let c = feed_string  t "HTTP/1.1 200 OK\r\nDate" in
  Alcotest.(check int) "read the status line" c 17;
  report_exn t (Failure "something went wrong");
  connection_is_shutdown t;
  Alcotest.(check (option string)) "something went wrong"
    (Some "something went wrong")
    !error_message
;;

let test_failed_response_parse () =
  let request' = Request.create `GET "/" in

  let test response bytes_read expected_error =
    let error = ref None in
    let body, t =
      request
        request'
        ~response_handler:(fun _ _ -> assert false)
        ~error_handler:(fun e -> error := Some e)
    in
    Body.close_writer body;
    write_request t request';
    writer_closed t;
    reader_ready t;
    let len = feed_string t response in
    Alcotest.(check int) "bytes read" len bytes_read;
    connection_is_shutdown t;
    Alcotest.(check (option response_error)) "Response error"
      (Some expected_error) !error;
  in

  test "HTTP/1.1 200\r\n\r\n" 12 (`Malformed_response ": char ' '");

  let response =
    Response.create `OK ~headers:(Headers.of_list ["Content-length", "-1"])
  in
  test (response_to_string response) 39 (`Invalid_response_body_length response);
;;

let tests =
  [ "GET"         , `Quick, test_get
  ; "Response EOF", `Quick, test_response_eof
  ; "Response header order preserved", `Quick, test_response_header_order
  ; "report_exn"  , `Quick, test_report_exn
  ; "input_shrunk", `Quick, test_input_shrunk
  ; "failed response parse", `Quick, test_failed_response_parse
  ]
