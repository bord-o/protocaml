open Eio

let addr = `Tcp (Net.Ipaddr.of_raw "\192\168\001\182", 8908)
(* let addr = `Tcp (Net.Ipaddr.of_raw "\192\168\001\211", 8908) *)
(* let addr = `Tcp (Net.Ipaddr.of_raw "\010\000\000\195", 8908) *)
(* ip: 131.150.169.142 *)
let rec handle_client buf flow db =
  (* this function is called by handler and handles the case of multiple requests on one connection*)
  let s = Buf_read.line buf in
  traceln "Request recieved ";

  try

         Flow.copy_string s flow;

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
