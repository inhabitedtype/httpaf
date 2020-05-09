open Httpaf
open Method

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
