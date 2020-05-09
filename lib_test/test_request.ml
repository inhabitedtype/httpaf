open Httpaf
open Request

let check =
  let alco =
    Alcotest.result
      (Alcotest.of_pp pp_hum)
      Alcotest.string
  in
  fun message ~expect input ->
    let actual =
      Angstrom.parse_string ~consume:All Httpaf_private.Parse.request input
    in
    Alcotest.check alco message expect actual
;;

let test_parse_valid () =
  check
    "valid GET without headers"
    ~expect:(Ok (Request.create `GET "/"))
    "GET / HTTP/1.1\r\n\r\n";
  check
    "valid non-standard method without headers"
    ~expect:(Ok (Request.create (`Other "some-other-verb") "/"))
    "some-other-verb / HTTP/1.1\r\n\r\n";
  check
    "valid GET with headers"
    ~expect:(Ok (Request.create ~headers:(Headers.of_list [ "Link", "/path/to/some/website"]) `GET "/"))
    "GET / HTTP/1.1\r\nLink: /path/to/some/website\r\n\r\n";
;;

let test_parse_invalid_errors () =
  check
    "doesn't end"
    ~expect:(Error ": not enough input")
    "GET / HTTP/1.1\r\n";
  check
    "invalid version"
    ~expect:(Error "eol: string")
    "GET / HTTP/1.22\r\n\r\n";
  check
    "malformed header"
    ~expect:(Error "header: char ':'")
    "GET / HTTP/1.1\r\nLink : /path/to/some/website\r\n\r\n";
;;

let tests =
  [ "parse valid"         , `Quick, test_parse_valid
  ; "parse invalid errors", `Quick, test_parse_invalid_errors
  ]
