open Eio

module Atomic_user_tbl = struct
  type ('a, 'b) t = { tbl : ('a, 'b) Hashtbl.t; mutex : Eio.Mutex.t }

  let of_hashtbl tbl = { tbl; mutex = Eio.Mutex.create () }

  (* wrappers for basic Hahstbl ops *)
  let add t user value =
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
        Hashtbl.add t.tbl user value)

  let _remove t user =
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
        Hashtbl.remove t.tbl user)

  let _mem t user =
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () -> Hashtbl.mem t.tbl user)

  let all_except_current t user =
    (* returns a list of all the users that are not the current user *)
    let aux tbl_user _ acc =
      if tbl_user <> user then tbl_user :: acc else acc
    in

    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
        let user_list = Hashtbl.fold aux t.tbl [] in
        user_list)
end

type switch_environment = { db : (string, int) Atomic_user_tbl.t }

(*let addr = `Tcp (Net.Ipaddr.of_raw "\192\168\001\182", 8908)*)
let addr = `Tcp (Net.Ipaddr.of_raw "\192\168\001\211", 8908)

(* let addr = `Tcp (Net.Ipaddr.of_raw "\010\000\000\195", 8908) *)
(* ip: 131.150.169.142 *)
let welcome = "Welcome to budgetchat! What shall I call you?\n"

let pp_string_list l =
  String.trim @@ List.fold_right (fun x y -> x ^ " " ^ y) l ""

let rec handle_req buf flow user_db cur_usr =
  (* this function is called by handler and handles the case of multiple requests on one connection*)
  let send str = Flow.copy_string str flow in
  let read () = Buf_read.line buf in

  try
    let name =
      match cur_usr with
      (* does the user have a name yet *)
      | Some u ->
          (* user has name so act as the normal chat application *)
          send @@ "Already signed in as " ^ u ^ "\n";
          let chat = read () in
          send @@ chat ^ "\n";
          u
      | None ->
          (*
             no name yet, so read in a string and add it to db
             TODO: check for duplicate before adding
          *)
          let s = read () in
          traceln "Request recieved ";
          traceln "adding %a to db" Fmt.string s;
          Atomic_user_tbl.add user_db (String.trim s) 1;
          let users = Atomic_user_tbl.all_except_current user_db s in
          let users_str = pp_string_list users in
          send ("* Room contains: " ^ users_str ^ "\n");
          s
    in

    traceln "Success in handler, still open to requests";
    handle_req buf flow user_db @@ Some name
  with e -> (
    let cleanup e =
      traceln "Error in handler: %a" Fmt.exn e;
      Flow.shutdown flow `All
    in
    match e with _ -> cleanup e)

let handle_conn ~sw_env flow _addr =
  traceln "handled connection, sending welcome message";
  Flow.copy_string welcome flow;
  (* welcome user and ask for name *)
  let buf = Buf_read.of_flow ~max_size:10000000 flow in
  (* make buffer for reading *)
  let user_db = sw_env.db in
  while not @@ Buf_read.at_end_of_input buf do
    (* keep handling requests until the user dc's or we get eof *)
    handle_req buf flow user_db None
  done

let main ~net ~addr =
  let main_error e =
    match e with
    | End_of_file -> ()
    | _ -> traceln "Error in run_server %a" Fmt.exn e
  in

  Switch.run @@ fun sw ->
  (*  *)
  let sock = Net.listen ~reuse_addr:true ~backlog:5 ~sw net addr in
  let user_db = Atomic_user_tbl.of_hashtbl (Hashtbl.create 100) in
  (*
     make table to track active users once per switch?
     xTODO: this needs a mutex. It is only tracking
     active users and will need to be sync at all times
  *)
  let sw_env = { db = user_db } in
  (* build the per-switch environment so we can pass it to the session handler *)
  Net.run_server ~max_connections:100 ~on_error:main_error sock
    (handle_conn ~sw_env)
(*
   so we need a way to broadcast to all active users without constantly polling for changes
   thinking of using a worker pool as per ocaml Eio examples.
   thinking of forking a worker for each connection in the (handle_conn ~sw_env) section above.
     this keeps the code pretty, although it is probably really inefficient to have so many workers (one per connection)
     in order to do this we need to add a work item stream to the sw_env and pass it to both the worker and the client handler.
     when the client performs an action where they need to broadcast (log in, sign of, send chat), we add the work to the stream.

   TODO: make sure that multiple workers wont grab the same stream item
   TODO: what stream capacity (probably unbounded is fine) but maybe zero capacity since there are so many workers.
   TODO: switch to Map instead of Hashtbl since I dont think that Flow is hashable and it needs to be included 
        in the global table for the workers to be able to access all of the client flows
*)

let () = Eio_main.run @@ fun env -> main ~net:(Stdenv.net env) ~addr

(*
   Chat
     user joins -> [name]
     [welcome ] <- server checks if name is in global table
*)
