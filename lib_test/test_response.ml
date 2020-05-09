open Httpaf
open Response

let check =
  let alco =
    Alcotest.result
      (Alcotest.of_pp pp_hum)
      Alcotest.string
  in
  fun message ~expect input ->
    let actual =
      Angstrom.parse_string ~consume:All Httpaf_private.Parse.response input
    in
    Alcotest.check alco message expect actual
;;

let test_parse_valid () =
  check
    "OK response without headers"
    ~expect:(Ok (Response.create `OK))
    "HTTP/1.1 200 OK\r\n\r\n";
;;

let test_parse_invalid_error () =
  check
    "OK response without a status message"
    ~expect:(Error ": char ' '")
    "HTTP/1.1 200\r\n\r\n";
  check
    "OK response without a status message"
    ~expect:(Error ": status-code empty")
    "HTTP/1.1 OK\r\n\r\n";
  check
    "OK response without a status message"
    ~expect:(Error ": status-code too long: \"999999937377999999999200\"")
    "HTTP/1.1 999999937377999999999200\r\n\r\n";
;;

let tests =
  [ "parse valid"        , `Quick, test_parse_valid
  ; "parse invalid error", `Quick, test_parse_invalid_error
  ]
