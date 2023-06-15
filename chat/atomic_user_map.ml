module UserMap = Map.Make (String)

type 'a t = { map : 'a UserMap.t; mutex : Eio.Mutex.t }

(* wrappers for basic Map ops *)
let of_map map = { map; mutex = Eio.Mutex.create () }

let add atom_map user value =
  Eio.Mutex.use_rw ~protect:false atom_map.mutex (fun () ->
      UserMap.add user value atom_map.map)

let remove atom_map user =
  Eio.Mutex.use_rw ~protect:false atom_map.mutex (fun () ->
      UserMap.remove user atom_map.map)

let mem atom_map user =
  Eio.Mutex.use_rw ~protect:false atom_map.mutex (fun () ->
      UserMap.mem user atom_map.map)

let all_except_current atom_map user =
  (* returns a list of all the users that are not the current user *)
  let aux map_user _ acc = if map_user <> user then map_user :: acc else acc in

  Eio.Mutex.use_rw ~protect:false atom_map.mutex (fun () ->
      let user_list = UserMap.fold aux atom_map.map [] in
      user_list)
