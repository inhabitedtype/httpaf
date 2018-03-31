open Httpaf
open Httpaf.Httpaf_private

let debug msg =
  if true then Printf.eprintf "%s\n%!" msg

let request_to_string r =
  let f = Faraday.create 0x1000 in
  Serialize.write_request f r;
  Faraday.serialize_to_string f

let response_to_string r =
  let f = Faraday.create 0x1000 in
  Serialize.write_response f r;
  Faraday.serialize_to_string f

let body_to_strings = function
  | `Empty       -> []
  | `Fixed   xs  -> xs
  | `Chunked xs  ->
    List.fold_right (fun x acc ->
      let len = String.length x in
      [Printf.sprintf "%x\r\n" len; x; "\r\n"] @ acc)
    xs [ "0\r\n" ]
;;

let case_to_strings = function
  | `Request  r, body -> [request_to_string  r] @ (body_to_strings body)
  | `Response r, body -> [response_to_string r] @ (body_to_strings body)

let response_stream_to_body (`Response response, body) =
  let response = response_to_string response in
  match body with
  | `Empty  -> response
  | `Fixed xs | `Chunked xs -> String.concat "" (response :: xs)

let iovec_to_string { IOVec.buffer; off; len } =
  Bigstring.to_string ~off ~len buffer

let bigstring_append_string bs s =
  Bigstring.of_string (Bigstring.to_string bs ^ s)

let bigstring_empty = Bigstring.of_string ""

let test_server ~input ~output ~handler () =
  let reads  = List.(concat (map case_to_strings input)) in
  let writes = List.(concat (map case_to_strings output)) in
  let conn   = Server_connection.create handler in
  let iwait, owait = ref false, ref false in
  let rec loop conn input reads =
    if !iwait && !owait then
      assert false (* deadlock, at lest for test handlers. *);
    if Server_connection.is_closed conn
    then begin
      debug "state: closed";
      []
    end else begin
      let input', reads' = iloop conn input reads in
      let output         = oloop conn in
      output @ loop conn input' reads'
    end
  and iloop conn input reads =
    if !iwait
    then begin debug " iloop: wait"; input, reads end
    else
      match Server_connection.next_read_operation conn, reads with
      | `Read, read::reads' ->
        debug " server iloop: read";
        let input     = bigstring_append_string input read in
        let input_len = Bigstring.length input in
        let result    = Server_connection.read conn input ~off:0 ~len:input_len in
        if result = input_len
        then bigstring_empty, reads'
        else Bigstring.sub ~off:result input, reads'
      | `Read, [] ->
        debug " server iloop: eof";
        Server_connection.shutdown_reader conn;
        bigstring_empty, []
      | _          , [] ->
        debug " server iloop: eof";
        Server_connection.shutdown_reader conn;
        bigstring_empty, []
      | `Close    , _     ->
        debug " server iloop: close(ok)"; input, []
      | `Yield , _  ->
        debug " server iloop: yield";
        iwait := true;
        Server_connection.yield_reader conn (fun () -> debug " iloop: continue"; iwait := false);
        input, reads
  and oloop conn =
    if !owait
    then (begin debug " server oloop: wait"; [] end)
    else
      match Server_connection.next_write_operation conn with
      | `Close _ ->
        debug " server oloop: closed"; []
      | `Yield ->
        debug " server oloop: yield";
        owait := true;
        Server_connection.yield_writer conn (fun () -> debug " server oloop: continue"; owait := false);
        []
      | `Write iovecs ->
        debug " server oloop: write";
        let output = List.map iovec_to_string iovecs in
        Server_connection.report_write_result conn (`Ok (IOVec.lengthv iovecs));
        output
  in
  let test_output = loop conn bigstring_empty reads |> String.concat "" in
  let output      = String.concat "" writes in
  Alcotest.(check string "response" output test_output)
;;

let test_client ~request ~request_body_writes ~response_stream () =
  let reads  = case_to_strings response_stream in
  let writes = case_to_strings (`Request request, `Fixed request_body_writes) in
  let test_input  = ref []    in
  let got_eof     = ref false in
  let error_handler _ = assert false in
  let response_handler response response_body =
    test_input := (response_to_string response) :: !test_input;
    let rec on_read bs ~off ~len =
      test_input := Bigstring.to_string bs ~off ~len :: !test_input;
      Body.schedule_read response_body ~on_read ~on_eof
    and on_eof () = got_eof := true in
    Body.schedule_read response_body ~on_read ~on_eof
  in
  let body, conn =
    Client_connection.request
      request
      ~error_handler
      ~response_handler
  in
  let rec loop conn request_body_writes input reads =
    if Client_connection.is_closed conn
    then []
    else begin
      let input', reads'               = iloop conn input reads in
      let output, request_body_writes' = oloop conn request_body_writes in
      output @ loop conn request_body_writes' input' reads'
    end
  and oloop conn request_body =
    let request_body' =
      match request_body with
      | []      -> Body.close_writer body; request_body
      | x :: xs -> Body.write_string body x; Body.flush body ignore; xs
    in
    match Client_connection.next_write_operation conn with
    | `Yield   ->
      (* This should only happen once to close the writer *)
      Client_connection.yield_writer conn ignore; [], request_body'
    | `Close _ ->
      debug " client oloop: closed"; [], request_body'
    | `Write iovecs ->
      debug " client oloop: write";
      let output = List.map iovec_to_string iovecs in
      Client_connection.report_write_result conn (`Ok (IOVec.lengthv iovecs));
      output, request_body'
  and iloop conn input reads =
    match Client_connection.next_read_operation conn, reads with
    | `Read, read::reads' ->
      debug " client iloop: read";
      let input     = bigstring_append_string input read in
      let input_len = Bigstring.length input in
      let result     = Client_connection.read conn input ~off:0 ~len:input_len in
      if result = input_len
      then bigstring_empty, reads'
      else Bigstring.sub ~off:result input, reads'
    | `Read, [] ->
      debug " client iloop: eof";
      Client_connection.shutdown_reader conn;
      input, []
    | _          , [] ->
      debug " client iloop: eof";
      Client_connection.shutdown_reader conn;
      input, []
    | `Close    , _     ->
      debug " client iloop: close(ok)";
      input, []
  in
  let test_output = loop conn request_body_writes bigstring_empty reads |> String.concat "" in
  let test_input  = List.rev !test_input |> String.concat "" in
  let input       = response_stream_to_body response_stream in
  let output      = String.concat "" writes in
  Alcotest.(check bool   "got eof"  true   !got_eof);
  Alcotest.(check string "request"  output test_output);
  Alcotest.(check string "response" input  test_input);
;;
