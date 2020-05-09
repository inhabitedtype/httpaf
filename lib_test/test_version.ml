open Httpaf
open Version

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
