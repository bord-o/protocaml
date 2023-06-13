open Eio

(*let addr = `Tcp (Net.Ipaddr.of_raw "\192\168\001\182", 8908)*)
let addr = `Tcp (Net.Ipaddr.of_raw "\192\168\001\211", 8908)
(* let addr = `Tcp (Net.Ipaddr.of_raw "\010\000\000\195", 8908) *)
(* ip: 131.150.169.142 *)
let welcome = "Welcome to budgetchat! What shall I call you?\n"

type switch_environment = {
  db: (string, int) Hashtbl.t
}

let rec handle_client buf flow user_db =
  (* this function is called by handler and handles the case of multiple requests on one connection*)
  let _send str = Flow.copy_string str flow in
  let _read = Buf_read.line buf in

  traceln "Request recieved ";

  try

        traceln "Success in handler, still open to requests";
        handle_client buf flow user_db

  with e -> (
    let cleanup e =
      traceln "Error in handler: %a" Fmt.exn e;
      Flow.shutdown flow `All
    in
    match e with _ -> cleanup e)

let handler ~sw_env flow _addr =
  traceln "handled connection, sending welcome message";
  Flow.copy_string welcome flow; (* welcome user and ask for name *)
  let buf = Buf_read.of_flow ~max_size:10000000 flow in (* make buffer for reading *)
  let user_db = sw_env.db in
  while not @@ Buf_read.at_end_of_input buf do
    handle_client buf flow user_db
  done

let main ~net ~addr =
  let main_error e =
    match e with
    | End_of_file -> ()
    | _ -> traceln "Error in run_server %a" Fmt.exn e
  in

  Switch.run @@ fun sw ->
  let sock = Net.listen ~reuse_addr:true ~backlog:5 ~sw net addr in
  let user_db = Hashtbl.create 100 in (* make user tbl once per switch? *)
  let sw_env = {
    db=  user_db;
  } in (* build the environment so we can pass it to the session handler *)
  Net.run_server ~max_connections:100 ~on_error:main_error sock (handler ~sw_env) 

let () = Eio_main.run @@ fun env -> main ~net:(Stdenv.net env) ~addr

(* 
Chat
  user joins -> [name]
  [welcome ] <- server checks if name is in global table

*)
