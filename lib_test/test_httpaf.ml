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
  let buffer = Bigstring.create 0

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

  let default_request_handler reqd =
    Reqd.respond_with_string reqd (Response.create `OK) ""
  ;;

  let test_initial_reader_state () =
    let t = create default_request_handler in
    Alcotest.(check (of_pp Read_operation.pp_hum)) "A new reader wants input"
      `Read (next_read_operation t);
  ;;

  let test_reader_is_closed_after_eof () =
    let t = create default_request_handler in
    let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
    Alcotest.(check int) "read_eof with no input returns 0" 0 c;
    Alcotest.(check (of_pp Read_operation.pp_hum)) "Shutting down a reader closes it"
      `Close (next_read_operation t);

    let t = create default_request_handler in
    let c = read t Bigstringaf.empty ~off:0 ~len:0 in
    Alcotest.(check int) "read with no input returns 0" 0 c;
    let c = read_eof t Bigstringaf.empty ~off:0 ~len:0; in
    Alcotest.(check int) "read_eof with no input returns 0" 0 c;
    Alcotest.(check (of_pp Read_operation.pp_hum)) "Shutting down a reader closes it"
      `Close (next_read_operation t);
  ;;

  let tests =
    [ "initial reader state"  , `Quick, test_initial_reader_state
    ; "shutdown reader closed", `Quick, test_reader_is_closed_after_eof
    ]

end

let () =
  Alcotest.run "httpaf unit tests"
    [ "version"          , Version.tests
    ; "method"           , Method.tests
    ; "iovec"            , IOVec.tests
    ; "server_connection", Server_connection.tests
    ]
