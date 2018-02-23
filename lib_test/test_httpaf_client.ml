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

let () =
  Alcotest.run "httpaf client tests"
    [ "GET" , get
    ; "POST", post ]
