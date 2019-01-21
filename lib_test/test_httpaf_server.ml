open Httpaf

let basic_handler body reqd =
  Simulator.debug " > handler called";
  let request_body = Reqd.request_body reqd in
  Body.close_reader request_body;
  Reqd.respond_with_string reqd (Response.create `OK) body;
;;

let echo_handler got_eof reqd =
  Simulator.debug " > echo_handler called";
  let request_body  = Reqd.request_body reqd in
  let response      = Response.create ~headers:Headers.(of_list ["connection", "close"]) `OK in
  let response_body = Reqd.respond_with_streaming reqd response in
  let rec on_read buffer ~off ~len =
    Body.write_string response_body (Bigstring.to_string ~off ~len buffer);
    Body.flush response_body (fun () ->
      Body.schedule_read request_body ~on_eof ~on_read)
  and on_eof () = got_eof := true; Body.close_writer response_body in
  Body.schedule_read request_body ~on_eof ~on_read;
;;

let single_get =
  [ "single GET"
    , `Quick
    , Simulator.test_server
        ~handler: (basic_handler "")
        ~input:   [(`Request (Request.create `GET "/")), `Empty]
        ~output:  [(`Response (Response.create `OK)   ), `Empty]
  ; "single GET, close connection"
    , `Quick
    , Simulator.test_server
        ~handler: (basic_handler "")
        ~input:   [ `Request (Request.create ~headers:Headers.(of_list ["connection", "close"]) `GET "/"), `Empty
                  ; `Request (Request.create `GET "/"), `Empty ]
        ~output:  [ `Response (Response.create `OK), `Empty ]

  ; "single GET with body"
  , `Quick
  , Simulator.test_server
      ~handler: (basic_handler "Hello, world!")
      ~input:   [ `Request (Request.create ~headers:Headers.(of_list ["connection", "close"]) `GET "/"), `Empty ]
      ~output:  [ `Response (Response.create `OK), `Fixed ["Hello, world!"] ]
  ; "single GET with streaming body"
  , `Quick
  , begin fun () ->
      let got_eof = ref false in
      Simulator.test_server ()
        ~handler: (echo_handler got_eof)
        ~input:   [ `Request (Request.create `POST "/" ~headers:Headers.(of_list ["transfer-encoding", "chunked"]))
                  , `Chunked ["This is a test"] ]
        ~output:  [`Response (Response.create `OK ~headers:Headers.(of_list ["connection", "close"]))
                  , `Fixed ["This is a test"] ];
      Alcotest.(check bool "got eof" !got_eof true);
    end
  ; "single GET with streaming body, multiple chunks"
  , `Quick
  , begin fun () ->
      let got_eof = ref false in
      Simulator.test_server ()
        ~handler: (echo_handler got_eof)
        ~input:   [ `Request (Request.create `POST "/" ~headers:Headers.(of_list ["transfer-encoding", "chunked"]))
                  , `Chunked
                      [ "This is a test"
                      ; " ... that involves multiple chunks"  ] ]
        ~output:  [`Response (Response.create `OK ~headers:Headers.(of_list ["connection", "close"]))
                  , `Fixed ["This is a test ... that involves multiple chunks"] ];
      Alcotest.(check bool "got eof" !got_eof true);
    end
  ]
;;

let multiple_gets =
  [ "multiple GETs"
    , `Quick
    , Simulator.test_server
        ~handler: (basic_handler "")
        ~input:   [ `Request (Request.create `GET "/"), `Empty
                  ; `Request (Request.create `GET "/"), `Empty ]
        ~output:  [ `Response (Response.create `OK), `Empty
                  ; `Response (Response.create `OK), `Empty ]
  ]
;;

let streaming_response =
  [ "streaming body with flush"
  , `Quick
  , Simulator.test_server
      ~handler: (fun reqd ->
        Simulator.debug " > handler called";
        let request_body = Reqd.request_body reqd in
        Body.close_reader request_body;
        let body = Reqd.respond_with_streaming reqd (Response.create `OK) in
        Body.write_string body "Hello,";
        Body.flush body (fun () ->
          Body.write_string body " world!";
          Body.close_writer body))
      ~input:   [ `Request (Request.create `GET "/"), `Empty ]
      ~output:  [ `Response (Response.create `OK), `Fixed ["Hello,"; " world!"] ]
  ; "streaming headers, flush immediately"
  , `Quick
  , Simulator.test_server
      ~handler: (fun reqd ->
        Simulator.debug " > handler called";
        let request_body = Reqd.request_body reqd in
        Body.close_reader request_body;
        let body = Reqd.respond_with_streaming ~flush_headers_immediately:true reqd (Response.create `OK) in
        Body.flush body (fun () ->
          Body.close_writer body))
      ~input:   [ `Request (Request.create ~headers:Headers.(of_list ["connection", "close"]) `GET "/"), `Empty ]
      ~output:  [ `Response (Response.create `OK), `Fixed [] ]
  ]

;;

let malformed_requests_tests = [
    "single GET, malformed request"
  , `Quick
  , Simulator.test_server
      ~handler: (basic_handler "")
      ~input:   [ `Raw [ "GET / HTTP/1.1\r\nconnection: close\r\nX-Other-Header : shouldnt_have_space_before_colon\r\n\r\n" ]
                , `Empty
                ]
      ~output:  [(`Response (Response.create `Bad_request)), `Fixed ["400"]]
]

let () =
  Alcotest.run "httpaf server tests"
    [ "single get"        , single_get
    ; "multiple gets"     , multiple_gets
    ; "streaming response", streaming_response
    ; "malformed requests", malformed_requests_tests
    ]
