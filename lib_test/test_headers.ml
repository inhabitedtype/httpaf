open Httpaf
module Array = ArrayLabels
module List = ListLabels

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

let test_ci_equal () =
  let string_of_char x = String.init 1 (fun _ -> x) in
  let ascii =
    Array.init (0xff + 1) ~f:Char.chr
    |> Array.to_list
  in
  let ascii_pairs =
    List.map ascii ~f:(fun x ->
      List.map ascii ~f:(fun y -> x, y))
    |> List.concat
  in
  (* Ensure that the branch free case-insensitive equality check is consistent
   * with a naive implementation. *)
  List.iter ascii_pairs ~f:(fun (x, y) ->
    let char_ci_equal =
      Char.compare (Char.lowercase_ascii x) (Char.lowercase_ascii y) = 0
    in
    let headers_equal =
      let headers = Headers.of_list [ string_of_char y, "value" ] in
      Headers.mem headers (string_of_char x)
    in
    Alcotest.(check bool)
      (Printf.sprintf "CI: %C = %C" x y)
      char_ci_equal
      headers_equal)
;;



let tests =
  [ "remove"  , `Quick, test_remove
  ; "replace" , `Quick, test_replace
  ; "CI equal", `Quick, test_ci_equal
  ]
