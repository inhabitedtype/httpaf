open Httpaf
open Response
open Helpers

let body_length = Alcotest.of_pp Response.Body_length.pp_hum

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

let test_body_length () =
  let check message request_method response ~expect =
    let actual = Response.body_length response ~request_method in
    Alcotest.check body_length message expect actual
  in
  let res status headers = Response.create status ~headers in
  check
    "requested HEAD"
    ~expect:(`Fixed 0L)
    `HEAD (res `OK Headers.empty);
  check
    "requested CONNECT"
    ~expect:(`Close_delimited)
    `CONNECT (res `OK Headers.empty);
  check
    "status: informational"
    ~expect:(`Fixed 0L)
    `GET (res `Continue Headers.empty);
  check
    "status: no content"
    ~expect:(`Fixed 0L)
    `GET (res `No_content Headers.empty);
  check
    "status: not modified"
    ~expect:(`Fixed 0L)
    `GET (res `Not_modified Headers.empty);
  check
    "no header"
    ~expect:(`Close_delimited)
    `GET (res `OK Headers.empty);
  check
    "single fixed"
    ~expect:(`Fixed 10L)
    `GET (res `OK Headers.(encoding_fixed 10));
  check
    "negative fixed"
    ~expect:(`Error `Internal_server_error)
    `GET (res `OK Headers.(encoding_fixed (-10)));
  check
    "multiple fixed"
    ~expect:(`Error `Internal_server_error)
    `GET (res `OK Headers.(encoding_fixed 10 @ encoding_fixed 20));
  check
    "chunked"
    ~expect:`Chunked
    `GET (res `OK Headers.encoding_chunked);
  check
    "chunked multiple times"
    ~expect:`Chunked
    `GET (res `OK Headers.(encoding_chunked @ encoding_chunked));
  let encoding_gzip = Headers.of_list ["transfer-encoding", "gzip"] in
  check
    "non-chunked transfer-encoding"
    ~expect:`Close_delimited
    `GET (res `OK encoding_gzip);
  check
    "chunked after non-chunked"
    ~expect:`Chunked
    `GET (res `OK Headers.(encoding_gzip @ encoding_chunked));
  check
    "chunked before non-chunked"
    ~expect:`Close_delimited
    `GET (res `OK Headers.(encoding_chunked @ encoding_gzip));
  check
    "chunked case-insensitive"
    ~expect:`Chunked
    `GET (res `OK Headers.(of_list ["transfer-encoding", "CHUNKED"]));
;;

let tests =
  [ "parse valid"        , `Quick, test_parse_valid
  ; "parse invalid error", `Quick, test_parse_invalid_error
  ; "body length"        , `Quick, test_body_length
  ]
