open Httpaf
open IOVec

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
  ; "shiftv raises ", `Quick, test_shiftv_raises
  ]
