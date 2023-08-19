open Eio

module Atomic_user_tbl = struct
  type ('a, 'b) t = { tbl : ('a, 'b) Hashtbl.t; mutex : Eio.Mutex.t }

  let of_hashtbl tbl = { tbl; mutex = Eio.Mutex.create () }

  (* wrappers for basic Hahstbl ops *)
  let add t user value =
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
        Hashtbl.add t.tbl user value)

  let remove t user =
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
        Hashtbl.remove t.tbl user)

  let find t user =
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () -> Hashtbl.find t.tbl user)

  let _mem t user =
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () -> Hashtbl.mem t.tbl user)

  let all_except_current t user =
    (* returns a list of all the users' flows that are not the current user *)
    let aux tbl_user _ acc =
      if tbl_user <> user then tbl_user :: acc else acc
    in

    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
        let user_list = Hashtbl.fold aux t.tbl [] in
        user_list)
end

type job = { message : string; recipients : Flow.two_way list }

type 'a switch_environment = {
  db : (string, 'a) Atomic_user_tbl.t;
  jobs : job Eio.Stream.t;
  sw : Eio.Switch.t;
}

(* let addr = `Tcp (Net.Ipaddr.of_raw "\192\168\001\182", 8908) *)
let addr = `Tcp (Net.Ipaddr.of_raw "\192\168\001\211", 8908)

(* let addr = `Tcp (Net.Ipaddr.of_raw "\010\000\000\195", 8908) *)
(* ip: 131.150.169.142 *)
let welcome = "Welcome to budgetchat! What shall I call you?\n"

let pp_string_list l =
  String.trim @@ List.fold_right (fun x y -> x ^ " " ^ y) l ""

exception UserDisconnect of string

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_alphnum c = is_alpha c || is_digit c

exception InvalidName of string

let valid_name name =
  String.for_all is_alphnum name
  && String.length name > 0
  && String.length name < 17

let rec handle_req buf flow sw_env cur_usr =
  (* this function is called by handler and handles the case of multiple requests on one connection*)
  let send str = Flow.copy_string str flow in
  let read () = Buf_read.line buf in
  let user_db = sw_env.db in
  let job_stream = sw_env.jobs in

  try
    let name =
      match cur_usr with
      (* does the user have a name yet *)
      | Some u ->
          (* user has name so act as the normal chat application *)
          (* send @@ "Already signed in as " ^ u ^ "\n"; *)
          let message = Printf.sprintf "[%s] %s\n" u @@ read () in
          let recipient_names = Atomic_user_tbl.all_except_current user_db u in
          let recipients =
            List.map (Atomic_user_tbl.find user_db) recipient_names
          in
          Eio.Stream.add job_stream { message; recipients };
          u
      | None ->
          (*
             no name yet, so read in a string and add it to db
             TODO: check for duplicate before adding
          *)
          let sraw = read () in
          let s = String.trim sraw in

          traceln "Request recieved ";
          let valid = valid_name s in
          if not valid then raise @@ InvalidName s else ();
          traceln "adding %a to db" Fmt.string s;
          Atomic_user_tbl.add user_db s flow;
          traceln "success adding to db";

          let message = Printf.sprintf "* %s has entered the room\n" s in
          let recipient_names = Atomic_user_tbl.all_except_current user_db s in
          let recipients =
            List.map (Atomic_user_tbl.find user_db) recipient_names
          in

          traceln "adding to job steam";
          Eio.Stream.add job_stream { message; recipients };
          traceln "success adding to job stream";

          let users = Atomic_user_tbl.all_except_current user_db s in
          let users_str = pp_string_list users in
          send ("* Room contains: " ^ users_str ^ "\n");
          s
    in

    traceln "Success in handler, still open to requests";
    handle_req buf flow sw_env @@ Some name
  with e -> (
    let cleanup e =
      traceln "Error in handler: %a" Fmt.exn e;
      Flow.shutdown flow `All
    in

    match e with
    | End_of_file ->
        (* remove user from table on dc  *)
        let n = Option.value ~default:"" cur_usr in
        traceln "removing %a from db" Fmt.string n;
        let message = Printf.sprintf "* %s has left the room\n" n in
        let recipient_names = Atomic_user_tbl.all_except_current user_db n in
        let recipients =
          List.map (Atomic_user_tbl.find user_db) recipient_names
        in
        Eio.Stream.add job_stream { message; recipients };

        Atomic_user_tbl.remove user_db n;

        cleanup @@ UserDisconnect n
    | e -> cleanup e)

let handle_worker ~sw_env =
  while true do
    let job = Eio.Stream.take sw_env.jobs in
    let send flow = Flow.copy_string job.message flow in
    traceln "broadcasting %a" Fmt.string job.message;
    List.iter (fun recipient -> send recipient) job.recipients
  done

let handle_conn ~sw_env flow _addr =
  traceln "handled connection, sending welcome message and starting worker";
  Fiber.fork ~sw:sw_env.sw (fun () -> handle_worker ~sw_env);
  Flow.copy_string welcome flow;
  (* welcome user and ask for name *)
  let buf = Buf_read.of_flow ~max_size:10000000 flow in
  let req_flow = flow in

  (* make buffer for reading *)
  while not @@ Buf_read.at_end_of_input buf do
    (* keep handling requests until the user dc's or we get eof *)
    handle_req buf req_flow sw_env None
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
  let sw_env = { db = user_db; jobs = Eio.Stream.create 100; sw } in
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
*)

let () = Eio_main.run @@ fun env -> main ~net:(Stdenv.net env) ~addr

(*
   Chat
     user joins -> [name]
     [welcome ] <- server checks if name is in global table
*)
