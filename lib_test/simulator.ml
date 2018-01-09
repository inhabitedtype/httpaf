open Httpaf
open Httpaf.Httpaf_private

let debug msg =
  if true then Printf.eprintf "%s\n%!" msg

type body_part = [ `Fixed of string | `Chunk of string ]

type request_stream  = [ `Request  of Request.t  | body_part ] list
type response_stream = [ `Response of Response.t | body_part ] list

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

let stream_to_strings ss =
  let rec loop ss acc =
    match ss with
    | []                      -> List.rev acc
    | `Request  r      :: ss' -> loop ss' (request_to_string r :: acc)
    | `Response r      :: ss' -> loop ss' (response_to_string r :: acc)
    | #body_part as bp :: ss' -> loop ss' (body_part_to_rev_strings bp @ acc)
  in
  loop ss []

let request_stream_to_strings  : request_stream  -> string list = stream_to_strings
let response_stream_to_strings : response_stream -> string list = stream_to_strings

let iovec_to_string { IOVec.buffer; off; len } =
  Bigstring.to_string ~off ~len buffer

let test_server ~input ~output ~handler () =
  let input  = request_stream_to_strings input in
  let output = response_stream_to_strings output in
  let conn   = Server_connection.create handler in
  let iwait, owait = ref false, ref false in
  let rec loop conn input =
    if !iwait && !owait then
      assert false (* deadlock, at lest for test handlers. *);
    if Server_connection.is_closed conn
    then begin
      debug "state: closed";
      []
    end else begin
      let input'  = iloop conn input in
      let output  = oloop conn in
      output @ loop conn input'
    end
  and iloop conn input =
    if !iwait
    then begin debug " iloop: wait"; input end
    else
      match Server_connection.next_read_operation conn, input with
      | `Read buffer, s::input' ->
        debug " iloop: read";
        let len = min (Bigstring.length buffer) (String.length s) in
        Bigstring.blit_from_string s 0 buffer 0 len;
        Server_connection.report_read_result conn (`Ok len);
        if len = String.length s
        then input'
        else String.(sub s len (length s - len)) :: input'
      | `Read _, [] ->
        debug " iloop: eof";
        Server_connection.report_read_result conn `Eof;
        []
      | _          , [] ->
        debug " iloop: eof";
        Server_connection.report_read_result conn `Eof;
        []
      | `Close    , _     ->
        debug " iloop: close(ok)"; []
      | `Yield , _  ->
        debug " iloop: yield";
        iwait := true;
        Server_connection.yield_reader conn (fun () -> debug " iloop: continue"; iwait := false);
        input
  and oloop conn =
    if !owait
    then (begin debug " oloop: wait"; [] end)
    else
      match Server_connection.next_write_operation conn with
      | `Close _ ->
        debug " oloop: closed"; []
      | `Yield ->
        debug " oloop: yield";
        owait := true;
        Server_connection.yield_writer conn (fun () -> debug " oloop: continue"; owait := false);
        []
      | `Write iovecs ->
        debug " oloop: write";
        let output = List.map iovec_to_string iovecs in
        Server_connection.report_write_result conn (`Ok (IOVec.lengthv iovecs));
        output
  in
  let test_output = loop conn input |> String.concat "" in
  let output      = String.concat "" output in
  Alcotest.(check string "response" output test_output)
;;

let test_client ~request ~request_body_writes ~response_stream () =
  let input  = response_stream_to_strings response_stream in
  let output =
    request_stream_to_strings
      ((`Request request) :: List.(map (fun x -> `Fixed x) request_body_writes))
  in
  let test_input  = ref []    in
  let got_eof     = ref false in
  let error_handler _ = assert false in
  let response_handler response response_body =
    test_input := (`Response response) :: !test_input;
    let chunk =
      match request.Request.meth with
      | #Method.standard as request_method ->
        begin match Response.body_length ~request_method response with
        | `Fixed _ | `Close_delimited   -> fun x -> `Fixed x
        | `Chunked                      -> fun x -> `Chunk x
        | _ -> assert false
        end
      | _ -> assert false
    in
    let rec on_read bs ~off ~len =
      let fixed = Bigstring.to_string bs ~off ~len in
      test_input := (chunk fixed) :: !test_input;
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
  let rec loop conn request_body_writes input =
    if Client_connection.is_closed conn
    then []
    else begin
      let input'                       = iloop conn input in
      let output, request_body_writes' = oloop conn request_body_writes in
      output @ loop conn request_body_writes' input'
    end
  and oloop conn request_body =
    let request_body' =
      match request_body with
      | []      -> Body.close body; request_body
      | x :: xs -> Body.write_string body x; Body.flush body ignore; xs
    in
    match Client_connection.next_write_operation conn with
    | `Yield   ->
      (* This should only happen once to close the writer *)
      Client_connection.yield_writer conn ignore; [], request_body'
    | `Close _ ->
      debug " oloop: closed"; [], request_body'
    | `Write iovecs ->
      debug " oloop: write";
      let output = List.map iovec_to_string iovecs in
      Client_connection.report_write_result conn (`Ok (IOVec.lengthv iovecs));
      output, request_body'
  and iloop conn input =
    match Client_connection.next_read_operation conn, input with
    | `Read buffer, s::input' ->
      debug " iloop: read";
      let len = min (Bigstring.length buffer) (String.length s) in
      Bigstring.blit_from_string s 0 buffer 0 len;
      Client_connection.report_read_result conn (`Ok len);
      if len = String.length s
      then input'
      else String.(sub s len (length s - len)) :: input'
    | `Read _, [] ->
      debug " iloop: eof";
      Client_connection.report_read_result conn `Eof;
      []
    | _          , [] ->
      debug " iloop: eof";
      Client_connection.report_read_result conn `Eof;
      []
    | `Close    , _     ->
      debug " iloop: close(ok)"; []
  in
  let test_output = loop conn request_body_writes input    |> String.concat "" in
  let test_input  = List.rev !test_input |> response_stream_to_strings |> String.concat "" in
  let input       = String.concat "" input in
  let output      = String.concat "" output in
  Alcotest.(check bool   "got eof"  true   !got_eof);
  Alcotest.(check string "request"  output test_output);
  Alcotest.(check string "response" input  test_input);
;;
