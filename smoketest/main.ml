open Eio.Net
open Eio

(* our sockaddr *)
let addr = `Tcp (Ipaddr.of_raw "\192\168\001\182", 8908)

(* loop for handling user connection *)
let rec client_loop flow histbuf =
  (* Make a buffer for reading from the connection with a 1MB max size *)
  let buf = Eio.Buf_read.of_flow flow ~initial_size:100 ~max_size:1_000_000 in

  if Buf_read.at_end_of_input buf then (
    (* If we get EOF then handle writing the history back to the user and tear down the connection *)
    let s = Buffer.contents histbuf in
    traceln "%s" s;
    Flow.copy_string s flow;
    Flow.shutdown flow `All)
  else
    (* Otherwise we read the entire buffer into a string and add it to our history buffer *)
    let s = Buf_read.take_all buf in
    traceln "%s" s;
    Buffer.add_string histbuf s;

    client_loop flow histbuf

(* Handle the state for the connection which is just the msg history, then start the client loop*)
let handle_client flow _addr =
  let history = Buffer.create 10000 in
  client_loop flow history

(* Eio boilerplate *)
let run_server socket =
  Net.run_server socket handle_client
    ~on_error:(traceln "Error handling connection: %a" Fmt.exn)

(* Eio Boilerplate x2 *)
let main ~net ~addr =
  Switch.run @@ fun sw ->
  let server = Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  run_server server

let () = Eio_main.run @@ fun env -> main ~net:(Stdenv.net env) ~addr
