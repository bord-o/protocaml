let range start stop step =
  let open Float in
  let len = div (of_int (stop - start)) (of_int step) |> ceil |> to_int in
  List.init len (fun i -> start + (i * step))

let isqrt n =
  let module F = Float in
  n |> F.of_int |> F.sqrt |> F.floor |> Int.of_float

let isPrime n =
  match n with
  | n when n <= 3 -> n > 1
  | n when n mod 2 = 0 || n mod 3 = 0 -> false
  | n ->
      let limit = isqrt n in
      let num_range = range 5 (limit + 1) 6 in
      let res =
        List.map
          (fun i -> if n mod i = 0 || n mod (i + 2) = 0 then false else true)
          num_range
      in
      if res = [] then true else List.mem false res |> not
