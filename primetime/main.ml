open Eio.Net
open Eio

(* 
Request:  
  single line with a json object terminated by  '\n'

  {"method":"isPrime", "number":123}

  Request is malformed if not valid json, if either field is missing,
  if the method is not "isprime" or if number is not a number type

  Ignore extra fields

Response:
  a single line json object terminated by '\n'

  has the method field containing "isPrime", and the field prime, which contains a boolean

  {"method": "isPrime", "prime": false}

  Response is malformed if not valid json, missing field, method isnot "isprime" or if prime is not bool
  Response is incorrect if the value of prime is wrong

Protocol:
  accept tcp connections (5 simul)
  
  Client -> connects tcp
  Client -> sends 1 or more valid requests, 
  Server <- send back response in order and wait for another request

  Client -> connects tcp
  Client -> sends malformed request
  Server <- send malformed request and disconnect
*)

(* our sockaddr *)
let addr = `Tcp (Ipaddr.of_raw "\010\000\000\195", 8908)
(*let addr = `Tcp (Ipaddr.of_raw "\192\168\001\019", 8908)*)
let range start stop step =
  let len = (Float.div (Float.of_int (stop - start)) (Float.of_int step)) |> Float.ceil |> Float.to_int  in
  List.init len (fun i -> start +(i*step))

let isqrt n = 
   n |> Float.of_int |> Float.sqrt |> Float.floor |> Int.of_float

let isPrime n = match n with
  | n when n <= 3 -> n > 1
  | n when (n mod 2 = 0) || (n mod 3 = 0) ->false
  | n -> (
      let limit = isqrt n in
      let num_range = range 5 (limit+1) 6 in
      let res = List.map (fun i ->
        if (n mod i = 0) || (n mod (i+2) = 0) then false
        else true) 
      num_range in
      if res = [] then true else
      List.mem false res |> not
      )
  

(* loop for handling user connection *)
type response_res = {malformed: bool; pmethod: string; number: float}
let rec client_loop flow =
  (* Make a buffer for reading from the connection with a 1MB max size *)
  let open Yojson.Basic.Util in
  let is_not_newline = (<>) '\n' in
  let is_not_end_or_newline c ~buf= is_not_newline c && not ( Buf_read.at_end_of_input buf)  in

  let buf = Buf_read.of_flow flow ~initial_size:100 ~max_size:1_000_000 in

  let s = Buf_read.take_while (is_not_end_or_newline ~buf) buf in (* the problem with multiple clients seems to be here *)

  traceln "%s" s;
  let json = 
    try Yojson.Basic.from_string s 
  with
    _ -> (
    (traceln "invalid json recieved...";
    Yojson.Basic.from_string "{\"method\": \"malformed\", \"prime\": false}\n"))
  in

  (* parse the method field, a fail to parse will result in None *)
  let json_method = match [json] |> filter_member "method" |> filter_string with
    | [] -> None
    | x::_ -> Some(x) in

  (* parse the number field into a float, a fail to parse will result in None *)
  (* TODO: deal with rounding or something *)
  let json_number = match [json] |> filter_member "number" |> filter_number with
    | [] -> None
    | x::_ -> Some(x) in

  (* check to see if request is malformed, ie either of the fields 
  didnt exist or didnt have the correct type*)
  let res = match json_method, json_number with
    | Some(m), Some(n) when m="isPrime"-> {malformed=false; pmethod=m; number=n}
    | _, _ -> {malformed=true; pmethod=""; number=0.} in 

  let {malformed; pmethod; number} = res in
  (* check if malformed *)
  if malformed then
    (traceln "malformed request recieved...";
    let response_payload = "{\"method\": \"malformed\", \"prime\": false}\n" in
    Flow.copy_string response_payload flow; (* send malformed response *)
    traceln "%s" (Printf.sprintf "Response payload (malformed): %s" response_payload);
    

    

    (*Flow.shutdown flow `All*)) (* shutdown conn *)
  else
  (
    traceln "%s" (Printf.sprintf "Method: %s, Number: %f, malformed %b" pmethod number malformed);
    let is_it_prime = if Float.is_integer number then
      (let parsed_number = Float.to_int number in
      isPrime parsed_number )
    else
      false
    in
    let response_payload = Printf.sprintf "{\"method\": \"isPrime\", \"prime\": %b}\n" is_it_prime in
    traceln "%s" (Printf.sprintf "Response payload: %s" response_payload);
    Flow.copy_string response_payload flow; (* send evaluated response *)
    client_loop flow
  )

   
(* start the client loop*)
let handle_client flow _addr =
  client_loop flow 

(* Eio boilerplate *)
(*
let run_server socket =
  Net.run_server socket handle_client
    ~on_error:(traceln "Error handling connection: %a" Fmt.exn)
*)

(* Eio Boilerplate x2 *)
let main ~net ~addr =
  Switch.run @@ fun sw ->
    let server = Net.listen net ~sw ~reuse_addr:true ~backlog:128 addr in
    while true do
      Net.accept_fork ~sw server ~on_error:(fun _ -> traceln "error on accept_fork") handle_client 
    done
  (*
  Fiber.fork ~sw (fun () -> 
    let server = Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
    run_server server)
  *)

let () = 
    Eio_main.run @@ fun env -> main ~net:(Stdenv.net env) ~addr
