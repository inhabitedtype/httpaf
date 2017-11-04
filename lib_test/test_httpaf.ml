open Httpaf
open Httpaf.Httpaf_private

let debug msg =
  if true then Printf.eprintf "%s\n%!" msg

type body_part = [ `Fixed of string | `Chunk of string ]

type input_stream  = [ `Request  of Request.t  | body_part ] list
type output_stream = [ `Response of Response.t | body_part ] list

let request_to_string r =
  let f = Faraday.create 0x1000 in
  Serialize.write_request f r;
  Faraday.serialize_to_string f

let response_to_string r =
  let f = Faraday.create 0x1000 in
  Serialize.write_response f r;
  Faraday.serialize_to_string f

let body_part_to_rev_strings = function
  | `Fixed x -> [x]
  | `Chunk x ->
    let len = String.length x in
    [x; Printf.sprintf "%x\r\n" len]

let input_stream_to_strings is =
  let rec loop is acc =
    match is with
    | []                      -> List.rev acc
    | `Request  r      :: is' -> loop is' (request_to_string r :: acc)
    | #body_part as bp :: is' -> loop is' (body_part_to_rev_strings bp @ acc)
  in
  loop is []

let output_stream_to_strings is =
  let rec loop is acc =
    match is with
    | []                       -> List.rev acc
    | `Response r       :: is' -> loop is' (response_to_string r :: acc)
    | #body_part  as bp :: is' -> loop is' (body_part_to_rev_strings bp @ acc)
  in
  loop is []

let iovec_to_string { IOVec.buffer; off; len } =
  Bigstring.to_string ~off ~len buffer

let test ~msg ~input ~output ~handler =
  let input  = input_stream_to_strings input in
  let output = output_stream_to_strings output in
  let conn   = Connection.create handler in
  let iwait, owait = ref false, ref false in
  let rec loop conn input =
    if !iwait && !owait then
      assert false (* deadlock, at lest for test handlers. *);
    match Connection.state conn with
    | `Running ->
      debug "state: running";
      let input'  = iloop conn input in
      let output  = oloop conn in
      output @ loop conn input'
    | `Closed_input ->
      debug "state: closed_input";
      let output = oloop conn in
      output
    | `Closed ->
      debug "state: closed";
      []
  and iloop conn input =
    if !iwait
    then begin debug " iloop: wait"; input end
    else
      match Connection.next_read_operation conn, input with
      | `Read buffer, s::input' ->
        debug " iloop: read";
        let len = min (Bigstring.length buffer) (String.length s) in
        Bigstring.blit_from_string s 0 buffer 0 len;
        Connection.report_read_result conn (`Ok len);
        if len = String.length s
        then input'
        else String.(sub s len (length s - len)) :: input'
      | `Read _, [] ->
        debug " iloop: eof";
        Connection.report_read_result conn `Eof;
        []
      | _          , [] ->
        debug " iloop: eof";
        Connection.report_read_result conn `Eof;
        []
      | `Close    , _     ->
        debug " iloop: close(ok)";
        Connection.shutdown_reader conn; []
      | `Yield , _  ->
        debug " iloop: yield";
        iwait := true;
        Connection.yield_reader conn (fun () -> debug " iloop: continue"; iwait := false);
        input
  and oloop conn =
    if !owait
    then (begin debug " oloop: wait"; [] end)
    else
      match Connection.next_write_operation conn with
      | `Close _ ->
        debug " oloop: closed";
        Connection.shutdown conn;
        []
      | `Yield ->
        debug " oloop: yield";
        owait := true;
        Connection.yield_writer conn (fun () -> debug " oloop: continue"; owait := false);
        []
      | `Write iovecs ->
        debug " oloop: write";
        let output = List.map iovec_to_string iovecs in
        Connection.report_write_result conn (`Ok (IOVec.lengthv iovecs));
        output
  in
  debug ("=== " ^ msg);
  let test_output = loop conn input in
  let output = String.concat "" output in
  let test_output = String.concat "" test_output in
  print_endline output;
  print_endline test_output;
  assert (output = test_output)
;;

let () =
  let handler body reqd =
    debug " > handler called";
    let request_body = Reqd.request_body reqd in
    Request.Body.close request_body;
    Reqd.respond_with_string reqd (Response.create `OK) body;
  in
  test ~msg:"Single OK" ~handler:(handler "")
    ~input:[`Request (Request.create `GET "/")]
    ~output:[`Response (Response.create `OK)];
  test ~msg:"Multiple OK" ~handler:(handler "")
    ~input:[`Request (Request.create `GET "/"); `Request (Request.create `GET "/")]
    ~output:[`Response (Response.create `OK); `Response (Response.create `OK)];
  test ~msg:"Conn close" ~handler:(handler "")
    ~input:[ `Request (Request.create ~headers:Headers.(of_list ["connection", "close"]) `GET "/")
           ; `Request (Request.create `GET "/")]
    ~output:[`Response (Response.create `OK)];
  test ~msg:"Single OK w/body" ~handler:(handler "Hello, world!")
    ~input:[ `Request (Request.create ~headers:Headers.(of_list ["connection", "close"]) `GET "/")]
    ~output:[`Response (Response.create `OK); `Fixed "Hello, world!" ];
  let echo reqd =
    debug " > handler called";
    let request_body  = Reqd.request_body reqd in
    let response_body = Reqd.respond_with_streaming reqd (Response.create ~headers:Headers.(of_list ["connection", "close"]) `OK) in
    let rec on_read buffer ~off ~len =
      Response.Body.write_string response_body (Bigstring.to_string ~off ~len buffer);
      Response.Body.flush response_body (fun () ->
        Request.Body.schedule_read request_body ~on_eof ~on_read);
      len
    and on_eof () = Response.Body.close response_body in
    Request.Body.schedule_read request_body ~on_eof ~on_read;
  in
  test ~msg:"POST" ~handler:echo
    ~input:[`Request (Request.create `GET "/" ~headers:Headers.(of_list ["transfer-encoding", "chunked"])); `Chunk "This is a test"]
    ~output:[`Response (Response.create `OK ~headers:Headers.(of_list ["connection", "close"])); `Fixed "This is a test"]
