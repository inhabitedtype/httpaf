open Httpaf

module Version = struct
  include Version

  let v1_0 = { major = 1; minor = 0 }
  let v1_1 = { major = 1; minor = 1 }

  let test_compare () =
    Alcotest.(check int) "compare v1_1 v1_0" (compare v1_1 v1_0) 1;
    Alcotest.(check int) "compare v1_1 v1_1" (compare v1_1 v1_1) 0;
    Alcotest.(check int) "compare v1_0 v1_0" (compare v1_0 v1_0) 0;
    Alcotest.(check int) "compare v1_0 v1_1" (compare v1_0 v1_1) (-1);
  ;;

  let test_to_string () =
    Alcotest.(check string) "to_string v1_1" (to_string v1_1) "HTTP/1.1";
    Alcotest.(check string) "to_string v1_0" (to_string v1_0) "HTTP/1.0";
  ;;

  let tests =
    [ "compare"  , `Quick, test_compare
    ; "to_string", `Quick, test_to_string
    ]
end

module Method = struct
  include Method

  let test_is_safe () =
    Alcotest.(check bool) "GET is safe"     (is_safe `GET )    true;
    Alcotest.(check bool) "HEAD is safe"    (is_safe `HEAD)    true;
    Alcotest.(check bool) "POST is safe"    (is_safe `POST)    false;
    Alcotest.(check bool) "PUT is safe"     (is_safe `PUT )    false;
    Alcotest.(check bool) "DELETE is safe"  (is_safe `DELETE ) false;
    Alcotest.(check bool) "CONNECT is safe" (is_safe `CONNECT) false;
    Alcotest.(check bool) "OPTIONS is safe" (is_safe `OPTIONS) true;
    Alcotest.(check bool) "TRACE is safe"   (is_safe `TRACE  ) true;
  ;;

  let test_is_cacheable () =
    Alcotest.(check bool) "GET is cacheable"     (is_cacheable `GET )    true;
    Alcotest.(check bool) "HEAD is cacheable"    (is_cacheable `HEAD)    true;
    Alcotest.(check bool) "POST is cacheable"    (is_cacheable `POST)    true;
    Alcotest.(check bool) "PUT is cacheable"     (is_cacheable `PUT )    false;
    Alcotest.(check bool) "DELETE is cacheable"  (is_cacheable `DELETE ) false;
    Alcotest.(check bool) "CONNECT is cacheable" (is_cacheable `CONNECT) false;
    Alcotest.(check bool) "OPTIONS is cacheable" (is_cacheable `OPTIONS) false;
    Alcotest.(check bool) "TRACE is cacheable"   (is_cacheable `TRACE  ) false;
  ;;

  let test_is_idempotent () =
    Alcotest.(check bool) "GET is idempotent"     (is_idempotent `GET )    true;
    Alcotest.(check bool) "HEAD is idempotent"    (is_idempotent `HEAD)    true;
    Alcotest.(check bool) "POST is idempotent"    (is_idempotent `POST)    false;
    Alcotest.(check bool) "PUT is idempotent"     (is_idempotent `PUT )    true;
    Alcotest.(check bool) "DELETE is idempotent"  (is_idempotent `DELETE ) true;
    Alcotest.(check bool) "CONNECT is idempotent" (is_idempotent `CONNECT) false;
    Alcotest.(check bool) "OPTIONS is idempotent" (is_idempotent `OPTIONS) true;
    Alcotest.(check bool) "TRACE is idempotent"   (is_idempotent `TRACE  ) true;
  ;;

  let tests =
    [ "is_safe"      , `Quick, test_is_safe
    ; "is_cacheable" , `Quick, test_is_cacheable
    ; "is_idempotent", `Quick, test_is_idempotent
    ]
end

module IOVec = struct
  include IOVec

  (* The length of the buffer is ignored by iovec operations *)
  let buffer = Bigstringaf.empty

  let test_lengthv () =
    Alcotest.(check int) "lengthv [] = 0"                 (lengthv []) 0;
    Alcotest.(check int) "lengthv [iovec] = length iovec"
      (lengthv [{ buffer; off = 0; len = 0 }]) (length {buffer; off = 0; len = 0 });
    Alcotest.(check int) "lengthv [iovec] = length iovec"
      (lengthv [{ buffer; off = 0; len = 10 }]) (length {buffer; off = 0; len = 10 });
  ;;

  let test_shiftv_raises () =
    Alcotest.check_raises 
      "IOVec.shiftv: -1 is a negative number"
      (Failure "IOVec.shiftv: -1 is a negative number")
      (fun () -> ignore (shiftv [] (-1)));
    let test f =
      Alcotest.check_raises
        "shiftv iovecs n raises when n > lengthv iovecs"
        (Failure "shiftv: n > lengthv iovecs")
      (fun () -> ignore (f ()))
    in
    test (fun () -> shiftv [] 1);
    test (fun () -> shiftv [{ buffer; off = 0; len = 1 }] 2);
    test (fun () -> shiftv [{ buffer; off = 0; len = 1 }; { buffer; off = 0; len = 1 }] 3);
  ;;

  let test_shiftv () =
    Alcotest.(check (of_pp pp_hum |> list)) "shiftv [] 0 = []" (shiftv [] 0) [];
    Alcotest.(check (of_pp pp_hum |> list)) "shiftv [{... len ... }] len = []"
      (shiftv [{ buffer; off = 0; len = 1 }] 1) [];
    Alcotest.(check (of_pp pp_hum |> list)) "shiftv [iovec] n when length iovec < n"
      (shiftv [{ buffer; off = 0; len = 4 }] 2) [{ buffer; off = 2; len = 2 }];
  ;;

  let tests =
    [ "lengthv"       , `Quick, test_lengthv
    ; "shiftv"        , `Quick, test_shiftv
    ; "shiftv raises ", `Quick, test_shiftv_raises ]
end

module Server_connection = struct
  include Server_connection

  module Read_operation = struct
    type t = [ `Read | `Yield | `Close ]

    let pp_hum fmt t =
      let str =
        match t with
        | `Read -> "Read"
        | `Yield -> "Yield"
        | `Close -> "Close"
      in
      Format.pp_print_string fmt str
    ;;
  end

  module Write_operation = struct
    type t = [ `Write of Bigstringaf.t IOVec.t list | `Yield | `Close of int ]

    let iovecs_to_string iovecs =
      let len = IOVec.lengthv iovecs in
      let bytes = Bytes.create len in
      let dst_off = ref 0 in
      List.iter (fun { IOVec.buffer; off = src_off; len } ->
        Bigstringaf.unsafe_blit_to_bytes buffer ~src_off bytes ~dst_off:!dst_off ~len;
        dst_off := !dst_off + len)
      iovecs;
      Bytes.unsafe_to_string bytes
    ;;

    let pp_hum fmt t =
      match t with
      | `Write iovecs -> Format.fprintf fmt "Write %S" (iovecs_to_string iovecs)
      | `Yield -> Format.pp_print_string fmt "Yield"
      | `Close len -> Format.fprintf fmt "Close %i" len
    ;;

    let to_write_as_string t =
      match t with
      | `Write iovecs -> Some (iovecs_to_string iovecs)
      | `Close _ | `Yield -> None
    ;;
  end

  let write_operation = Alcotest.of_pp Write_operation.pp_hum
  let read_operation = Alcotest.of_pp Read_operation.pp_hum
  let get_request_string = "GET / HTTP/1.1\r\n\r\n"

  let read_string t str =
    let len = String.length str in
    let input = Bigstringaf.of_string str ~off:0 ~len in
    read t input ~off:0 ~len;
  ;;

  let default_request_handler reqd =
    Reqd.respond_with_string reqd (Response.create `OK) ""
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
    Alcotest.check read_operation "Shutting down a reader closes it"
      `Close (next_read_operation t);

    let t = create default_request_handler in
    let c = read t Bigstringaf.empty ~off:0 ~len:0 in
    Alcotest.(check int) "read with no input returns 0" 0 c;
    let c = read_eof t Bigstringaf.empty ~off:0 ~len:0; in
    Alcotest.(check int) "read_eof with no input returns 0" 0 c;
    Alcotest.check read_operation "Shutting down a reader closes it"
      `Close (next_read_operation t);
  ;;

  let test_synchronous_error () =
    let writer_woken_up = ref false in
    let t = create ~error_handler synchronous_raise in
    Alcotest.check write_operation "Writer is in a yield state"
      `Yield (next_write_operation t);
    yield_writer t (fun () -> writer_woken_up := true);
    let c = read_string t get_request_string in
    Alcotest.(check int) "read consumes all input"
      (String.length get_request_string) c;
    Alcotest.check read_operation "Error shuts down the reader"
      `Close (next_read_operation t);
    Alcotest.(check bool) "Writer woken up"
      true !writer_woken_up;
    Alcotest.(check (option string)) "Error response written"
      (Some "HTTP/1.1 500 Internal Server Error\r\n\r\ngot an error")
      (next_write_operation t |> Write_operation.to_write_as_string)
  ;;

  let test_synchronous_error_asynchronous_handling () =
    let writer_woken_up = ref false in
    let continue = ref (fun () -> ()) in
    let error_handler ?request error start_response =
      continue := (fun () ->
        error_handler ?request error start_response)
    in
    let t = create ~error_handler synchronous_raise in
    Alcotest.check write_operation "Writer is in a yield state"
      `Yield (next_write_operation t);
    yield_writer t (fun () -> writer_woken_up := true);
    let c = read_string t get_request_string in
    Alcotest.(check int) "read consumes all input"
      (String.length get_request_string) c;
    Alcotest.check read_operation "Error shuts down the reader"
      `Close (next_read_operation t);
    Alcotest.check write_operation "Writer is in a yield state"
      `Yield (next_write_operation t);
    !continue ();
    Alcotest.(check bool) "Writer woken up"
      true !writer_woken_up;
    Alcotest.(check (option string)) "Error response written"
      (Some "HTTP/1.1 500 Internal Server Error\r\n\r\ngot an error")
      (next_write_operation t |> Write_operation.to_write_as_string)
  ;;


  let test_asynchronous_error () =
    let continue = ref (fun () -> ()) in
    let asynchronous_raise reqd =
      continue := (fun () -> synchronous_raise reqd)
    in
    let writer_woken_up = ref false in
    let t = create ~error_handler asynchronous_raise in
    Alcotest.check write_operation "Writer is in a yield state"
      `Yield (next_write_operation t);
    yield_writer t (fun () -> writer_woken_up := true);
    let c = read_string t get_request_string in
    Alcotest.(check int) "read consumes all input"
      (String.length get_request_string) c;
    Alcotest.check write_operation "Writer is in a yield state"
      `Yield (next_write_operation t);
    Alcotest.check read_operation "Reader is in a read state"
      `Yield (next_read_operation t);
    !continue ();
    Alcotest.check read_operation "Error shuts down the reader"
      `Close (next_read_operation t);
    Alcotest.(check bool) "Writer woken up"
      true !writer_woken_up;
    Alcotest.(check (option string)) "Error response written"
      (Some "HTTP/1.1 500 Internal Server Error\r\n\r\ngot an error")
      (next_write_operation t |> Write_operation.to_write_as_string)
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
    Alcotest.check write_operation "Writer is in a yield state"
      `Yield (next_write_operation t);
    yield_writer t (fun () -> writer_woken_up := true);
    let c = read_string t get_request_string in
    Alcotest.(check int) "read consumes all input"
      (String.length get_request_string) c;
    Alcotest.check write_operation "Writer is in a yield state"
      `Yield (next_write_operation t);
    Alcotest.check read_operation "Reader is in a read state"
      `Yield (next_read_operation t);
    !continue_request ();
    Alcotest.check write_operation "Writer is in a yield state"
      `Yield (next_write_operation t);
    !continue_error ();
    Alcotest.check read_operation "Error shuts down the reader"
      `Close (next_read_operation t);
    Alcotest.(check bool) "Writer woken up"
      true !writer_woken_up;
    Alcotest.(check (option string)) "Error response written"
      (Some "HTTP/1.1 500 Internal Server Error\r\n\r\ngot an error")
      (next_write_operation t |> Write_operation.to_write_as_string)
  ;;


  let tests =
    [ "initial reader state"  , `Quick, test_initial_reader_state
    ; "shutdown reader closed", `Quick, test_reader_is_closed_after_eof
    ; "synchronous error, synchronous handling", `Quick, test_synchronous_error
    ; "synchronous error, asynchronous handling", `Quick, test_synchronous_error_asynchronous_handling
    ; "asynchronous error, synchronous handling", `Quick, test_asynchronous_error
    ; "asynchronous error, asynchronous handling", `Quick, test_asynchronous_error_asynchronous_handling
    ]

end

let () =
  Alcotest.run "httpaf unit tests"
    [ "version"          , Version.tests
    ; "method"           , Method.tests
    ; "iovec"            , IOVec.tests
    ; "server_connection", Server_connection.tests
    ]
