open Yojson.Safe.Util


let addr = `Tcp (Eio.Net.Ipaddr.of_raw "\192\168\001\019", 8908)

module P = struct
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
end

let malformed_payload = "{\"method\": \"malformed\", \"prime\": false}\n"

exception Malformed of string
exception IncorrectMethod of string
exception InvalidJson

let string_parse (json:string) = 
  let s = 
    try Yojson.Safe.from_string json
    with _ -> (
      (* any parsing exception counts as malfomed *)
      raise InvalidJson
    ) in
  s
let json_parse (json: Yojson.Safe.t list) mem parser = 
  match json |> filter_member mem |> parser with
  | [] -> None
  | x::_ -> Some(x)

let validate_fields meth num =
  match meth, num with
  | Some(m), Some(_) when not @@ (String.equal m "isPrime") -> raise @@ IncorrectMethod m
  | Some(m), Some(n) -> m, n
  | None, Some(_) -> raise @@ Malformed "method"
  | Some(_), None -> raise @@ Malformed "number"
  | _, _ -> raise @@ Malformed "all fields"

let pmethod json = json_parse json "method" filter_string
let pnumber json = json_parse json "number" filter_number

let send_json (json: string) flow = 
  Eio.Flow.copy_string json flow

let send_malformed flow = send_json malformed_payload flow
let send_success is_prime flow = send_json (Printf.sprintf "{\"method\": \"isPrime\", \"prime\": %b}\n" is_prime ) flow

let rec handle_client buf flow = 
  let s = Eio.Buf_read.line buf in
  Eio.traceln "Req: %a" Fmt.string s;

  try
    let parse_res = string_parse s in
    let rnumber' = pnumber [parse_res] in
    let rmethod' = pmethod [parse_res] in

    let (_, rnumber ) = validate_fields rmethod'  rnumber' in
  
    let is_it_prime = if Float.is_integer rnumber then
      (let parsed_number = Float.to_int rnumber in
      P.isPrime parsed_number )
    else
      false
    in

    send_success is_it_prime flow;
    Eio.traceln "Success in handler, still open to requests";
    handle_client buf flow


  with e -> 
    let cleanup e = 
      Eio.traceln "Error in handler: %a" Fmt.exn  e;
      send_malformed flow;
      Eio.Flow.shutdown flow `All
    in
    match e with _ -> cleanup e

let handler flow _addr = 

  Eio.traceln "handled connection" ;
  let buf = Eio.Buf_read.of_flow ~max_size:1000 flow in
  while not @@ Eio.Buf_read.at_end_of_input buf do

    handle_client buf flow
  done
  

  (* ok so we have two main paths. send a successful response
    or send a malformed response
  *)
  (*
  Eio.Flow.copy_string "Hi from server" flow;
  Eio.Time.sleep clock 5.0;
  Eio.Flow.copy_string "Bye from server" flow;
  *)


let main ~net ~addr = 
  let main_error e = match e with
  | End_of_file -> ()
  | _ -> Eio.traceln "Error in run_server %a" Fmt.exn e in

  Eio.Switch.run @@ fun sw ->
  let sock = Eio.Net.listen ~backlog: 5 ~sw net addr in
    Eio.Net.run_server ~max_connections: 100 ~on_error:main_error sock (handler)


let () =
  Eio_main.run @@
    fun env -> main  ~net:(Eio.Stdenv.net env) ~addr