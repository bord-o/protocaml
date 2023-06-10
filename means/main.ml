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
let display_of_req r =
  let {op;i;j} = r in
  let ds = 
  (if op=Query then "Query" else "Insert") ^
  (Int32.to_string i) ^
  (Int32.to_string j)
  in ds

let rec handle_client buf flow =
  (* this function is called by handler and handles the case of multiple requests on one connection*)
  let s =  Buf_read.take 9 buf in
  traceln "Req: %a" Fmt.string s;

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
      Int32.of_string @@ "0b" ^ f
    in
    let n2 = 
      let f = String.sub s 5 4  in
      Int32.of_string @@ "0b" ^ f
    in
    let new_req = {op=op; i=n1;j=n2} in
    let ds = display_of_req new_req in
    traceln "New request built: %a" Fmt.string ds;
    Flow.copy_string s flow;
    traceln "Success in handler, still open to requests";

    handle_client buf flow
  with e -> (
    let cleanup e =
      traceln "Error in handler: %a" Fmt.exn e;
      Flow.shutdown flow `All
    in
    match e with _ -> cleanup e)

let handler flow _addr =
  traceln "handled connection";
  let buf = Buf_read.of_flow ~max_size:10000000 flow in
  while not @@ Buf_read.at_end_of_input buf do
    handle_client buf flow
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
