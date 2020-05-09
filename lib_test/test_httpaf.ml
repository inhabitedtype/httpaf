let () =
  Alcotest.run "httpaf unit tests"
    [ "version"          , Test_version.tests
    ; "method"           , Test_method.tests
    ; "iovec"            , Test_iovec.tests
    ; "headers"          , Test_headers.tests
    ; "request"          , Test_request.tests
    ; "response"         , Test_response.tests
    ; "client connection", Test_client_connection.tests
    ; "server connection", Test_server_connection.tests
    ]
