open Httpaf

let check msg ~expect actual =
  Alcotest.(check (list (pair string string))) msg expect (Headers.to_list actual)
;;

let test_replace () =
  check "replace trailing element"
    ~expect:["c", "d"; "a", "d"]
    (Headers.replace
      (Headers.of_list ["c", "d"; "a", "b"])
      "a"
      "d");

  check "replace middle element"
    ~expect:["e", "f"; "c", "z"; "a", "b"]
    (Headers.replace
       (Headers.of_list ["e", "f"; "c", "d"; "a", "b"])
       "c"
       "z");

  check "remove multiple trailing elements"
    ~expect:["c", "d"; "a", "d"]
    (Headers.replace
      (Headers.of_list [ "c", "d"; "a", "b"; "a", "c"])
      "a"
      "d");
;;

let test_remove () =
  check "remove leading element"
    ~expect:["c", "d"]
    (Headers.remove
      (Headers.of_list ["a", "b"; "c", "d"])
      "a");
  check "remove trailing element"
    ~expect:["c", "d"]
    (Headers.remove
      (Headers.of_list ["c", "d"; "a", "b"])
      "a");
;;

let tests =
  [ "remove" , `Quick, test_remove
  ; "replace", `Quick, test_replace
  ]
