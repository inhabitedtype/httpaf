open Httpaf


let get  =
  [ "GET with no response body"
    , `Quick
    , Simulator.test_client
        ~request:(Request.create `GET "/")
        ~request_body_writes:[]
        ~response_stream:(`Response (Response.create `OK), `Empty)
  ; "GET with a response body, chunked encoding"
    , `Quick
    , Simulator.test_client
        ~request:(Request.create `GET "/")
        ~request_body_writes:[]
        ~response_stream:(`Response (Response.create `OK
                                       ~headers:Headers.(of_list ["transfer-encoding", "chunked"])),
                           `Chunked ["Hello, world!"])
  ]

let post = []

let malformed_responses_tests = [
  "single GET, immediate EOF"
  , `Quick
  , Simulator.test_client_errors
      ~request:(Request.create `GET "/")
      ~request_body_writes:[]
      ~response_stream:(`Raw [""], `Empty)
]

let () =
  Alcotest.run "httpaf client tests"
    [
      "GET" , get
    ; "POST", post
    ; "Malformed responses", malformed_responses_tests
    ]
