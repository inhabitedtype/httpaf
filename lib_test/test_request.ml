open Httpaf
open Request
open Helpers

let body_length = Alcotest.of_pp Request.Body_length.pp_hum

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

let test_body_length () =
  let check message request ~expect =
    let actual = Request.body_length request in
    Alcotest.check body_length message expect actual
  in
  let req method_ headers = Request.create method_ ~headers "/" in
  check
    "no headers"
    ~expect:(`Fixed 0L)
    (req `GET Headers.empty);
  check
    "single fixed"
    ~expect:(`Fixed 10L)
    (req `GET Headers.(encoding_fixed 10));
  check
    "negative fixed"
    ~expect:(`Error `Bad_request)
    (req `GET Headers.(encoding_fixed (-10)));
  check
    "multiple fixed"
    ~expect:(`Error `Bad_request)
    (req `GET Headers.(encoding_fixed 10 @ encoding_fixed 20));
  check
    "chunked"
    ~expect:`Chunked
    (req `GET Headers.encoding_chunked);
  check
    "chunked multiple times"
    ~expect:`Chunked
    (req `GET Headers.(encoding_chunked @ encoding_chunked));
  let encoding_gzip = Headers.of_list ["transfer-encoding", "gzip"] in
  check
    "non-chunked transfer-encoding"
    ~expect:(`Error `Bad_request)
    (req `GET encoding_gzip);
  check
    "chunked after non-chunked"
    ~expect:`Chunked
    (req `GET Headers.(encoding_gzip @ encoding_chunked));
  check
    "chunked before non-chunked"
    ~expect:(`Error `Bad_request)
    (req `GET Headers.(encoding_chunked @ encoding_gzip));
  check
    "chunked case-insensitive"
    ~expect:`Chunked
    (req `GET Headers.(of_list ["transfer-encoding", "CHUNKED"]));
;;


let tests =
  [ "parse valid"         , `Quick, test_parse_valid
  ; "parse invalid errors", `Quick, test_parse_invalid_errors
  ; "body length",          `Quick, test_body_length
  ]
