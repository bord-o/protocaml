open Eio
(*let addr = `Tcp (Eio.Net.Ipaddr.of_raw "\192\168\001\019", 8908)*)
let addr = `Tcp (Net.Ipaddr.of_raw "\010\000\000\195", 8908)

(*
let chars_of_str s =
  String.fold_right (fun c l  -> c::l) s []
*)

exception RequestLength of int
exception UnknownOperation of char
type operation = Insert | Query
type req = {
  op: operation;
  i: int32;
  j: int32
}
type asset_price = {
  timestamp: int32;
  price: int32
}
let display_of_req r =
  let {op;i;j} = r in
  let ds = 
  (if op=Query then "Query" else "Insert") ^ " " ^
  (Int32.to_string i) ^ " " ^
  (Int32.to_string j)
  in ds

let rec handle_client buf flow db =
  (* this function is called by handler and handles the case of multiple requests on one connection*)
  let s =  Buf_read.take 9 buf in
  traceln "Request recieved ";

  try
    let l = String.length s in
    if l <> 9 then raise @@ RequestLength l else ();

    let op = 
      let first = String.get s 0 in
      match first with
      | 'Q' | 'I' -> if first='Q' then Query else Insert
      | x -> raise @@ UnknownOperation x
    in
    let n1 = 
      let f = String.sub s 1 4  in
      String.get_int32_be f 0
    in
    let n2 = 
      let f = String.sub s 5 4  in
      String.get_int32_be f 0
    in
    
    let new_req = {op=op; i=n1;j=n2} in
    let ds = display_of_req new_req in
    traceln "New request: %a" Fmt.string ds;

    (* 
    TODO: all of the logic goes here.
    we have two paths, Query or Insert. we will pattern match these cases, and just write the happy path,
    as exceptions are handled below.

    if Insert:
      insert a pair of time, and money into a strucure defined 
      somewhere where it will be availible to only the client on the respective connection
      (TODO: where should i define the memory, and what DS? )
      dont need to send anything back. just leave the connection alive
      
    if Query:
      filter our memory structure to only the entries that are >= the timestamp corresponding to 'n1', and <= to 'n2'
      these can just be left as int32s i think

      with the group of entries selected, we can just average the money field of them and construct our response 
      send our response of 4 raw bytes representing the average as an int32
      *)
    match op with
    | Insert -> (
      let asset = {timestamp=n1; price=n2} in
      Hashtbl.add db asset.timestamp asset.price;
      traceln "inserted asset into table"
        
    )
    | Query -> (
      let time1 = n1 in
      let time2 = n2 in
      let table_aux timestamp price acc =
        if timestamp <= time2 && timestamp >= time1 then price::acc else acc
      in
        
      let results = Hashtbl.fold table_aux db [] in
        let mean = try Int32.div (List.fold_left (Int32.add) 0l results) (Int32.of_int(List.length results)) with End_of_file -> 0l in

      traceln "calculated mean: %i" @@ Int32.to_int mean;
      let return_bytes = Bytes.make 4 'X' in
      Bytes.set_int32_be return_bytes 0 mean;
      let return_string = String.of_bytes return_bytes in
      Flow.copy_string return_string flow

    );
  
    traceln "Success in handler, still open to requests";

    handle_client buf flow db
  with e -> (
    let cleanup e =
      traceln "Error in handler: %a" Fmt.exn e;
      Flow.shutdown flow `All
    in
    match e with _ -> cleanup e)

let handler flow _addr =
  traceln "handled connection";
  let buf = Buf_read.of_flow ~max_size:10000000 flow in
  let db = Hashtbl.create 100 in
  while not @@ Buf_read.at_end_of_input buf do
    handle_client buf flow db
  done

let main ~net ~addr =
  let main_error e =
    match e with
    | End_of_file -> ()
    | _ -> traceln "Error in run_server %a" Fmt.exn e
  in

  Switch.run @@ fun sw ->
  let sock = Net.listen ~backlog:5 ~sw net addr in
  Net.run_server ~max_connections:100 ~on_error:main_error sock handler

let () = Eio_main.run @@ fun env -> main ~net:(Stdenv.net env) ~addr
