open Base

let parse_input lines =
  let rec aux lines locks keys =
    match lines with
    | [] -> (locks, keys)
    | _ ->
        let rest = List.drop lines 8 in
        let schema =
          List.take lines 7 |> List.map ~f:String.to_list |> List.rev
          |> List.transpose_exn
        in
        if String.equal (List.hd_exn lines) "#####" then
          aux rest
            (List.map schema ~f:(fun lst ->
                 List.count lst ~f:(Char.equal '#') - 1)
            :: locks)
            keys
        else
          aux rest locks
            (List.map schema ~f:(fun lst ->
                 List.count lst ~f:(Char.equal '#') - 1)
            :: keys)
  in
  aux lines [] []

let is_compatible lock key =
  List.for_all2_exn lock key ~f:(fun l k -> l + k <= 5)

let solve_part1 input =
  let locks, keys = parse_input input in
  List.fold locks ~init:0 ~f:(fun acc lock ->
      acc + List.count keys ~f:(is_compatible lock))
  |> Int.to_string

let solve_part2 _input = "Merry Christmas!"
