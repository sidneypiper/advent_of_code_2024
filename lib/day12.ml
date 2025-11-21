open Base

module PairInt = struct
  module T = struct
    type t = int * int

    let compare (x1, y1) (x2, y2) =
      match Int.compare x1 x2 with 0 -> Int.compare y1 y2 | cmp -> cmp

    let sexp_of_t (x, y) = Sexp.List [ Int.sexp_of_t x; Int.sexp_of_t y ]
  end

  include T
  include Comparator.Make (T)
end

type char_map = (int * int) Map.M(PairInt).t

let char_lists = List.map ~f:String.to_list

let update_map (map : char_map) (key : int * int) (values : int * int) :
    char_map =
  Map.change map key ~f:(function
    | None -> Some values
    | Some (a, p) ->
        let da, dp = values in
        Some (a + da, p + dp))

let print_map (map : char_map) =
  Map.iteri map ~f:(fun ~key:(x, y) ~data:(gx, gy) ->
      Stdio.printf "Key: (%d, %d) -> Value: (%d, %d)\n" x y gx gy)

let print_tuple tuple =
  let x, y = tuple in
  Stdio.printf "(%d, %d)" x y

let check_position (garden : char list list) (x : int) (y : int) : int * int =
  let current_char = List.nth_exn (List.nth_exn garden y) x in
  let is_different (nx, ny) =
    try Char.(List.nth_exn (List.nth_exn garden ny) nx <> current_char)
    with _ -> true
  in

  let neighbors = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ] in

  let diff_neighbors = List.count neighbors ~f:is_different in

  (1, diff_neighbors)

let determine_group (map : char_map) (garden : char list list) (x : int)
    (y : int) : int * int =
  let char_at (x, y) =
    try Some (List.nth_exn (List.nth_exn garden y) x) with _ -> None
  in

  let start_char = Option.value_exn (char_at (x, y)) in
  let visited = ref (Map.empty (module PairInt)) in
  let queue = Queue.create () in
  Queue.enqueue queue (x, y);

  let rec bfs () =
    match Queue.dequeue queue with
    | None -> None
    | Some (cx, cy) ->
        if Map.mem !visited (cx, cy) then bfs ()
        else (
          visited := Map.set !visited ~key:(cx, cy) ~data:();
          if Map.mem map (cx, cy) then Some (cx, cy)
          else
            let neighbors =
              [ (cx - 1, cy); (cx + 1, cy); (cx, cy - 1); (cx, cy + 1) ]
            in
            List.iter neighbors ~f:(fun (nx, ny) ->
                match char_at (nx, ny) with
                | Some c
                  when Char.(c = start_char) && not (Map.mem !visited (nx, ny))
                  ->
                    Queue.enqueue queue (nx, ny)
                | _ -> ());
            bfs ())
  in

  match bfs () with Some group_key -> group_key | None -> (x, y)

let solve_part1 (input : string list) =
  let garden = char_lists input in
  let map = ref (Map.empty (module PairInt)) in

  for x = 0 to List.length garden - 1 do
    for y = 0 to List.length garden - 1 do
      let group = determine_group !map garden x y in
      let update = check_position garden x y in
      map := update_map !map group update
    done
  done;

  List.map (Map.data !map) ~f:(fun (x, y) -> x * y)
  |> List.fold ~init:0 ~f:( + ) |> Int.to_string

let check_position_part2 (garden : char list list) (x : int) (y : int) :
    int * int =
  let current_char = List.nth_exn (List.nth_exn garden y) x in

  let is_different (nx, ny) =
    try Char.(List.nth_exn (List.nth_exn garden ny) nx <> current_char)
    with _ -> true
  in

  let neighbors =
    ( is_different (x, y - 1),
      is_different (x + 1, y),
      is_different (x, y + 1),
      is_different (x - 1, y) )
  in
  let diagonals =
    ( is_different (x + 1, y - 1),
      is_different (x + 1, y + 1),
      is_different (x - 1, y + 1),
      is_different (x - 1, y - 1) )
  in

  match neighbors with
  | false, false, false, false ->
      let a, b, c, d = diagonals in
      let tmp = [ a; b; c; d ] in
      let conv =
        List.map tmp ~f:(fun x -> if x then 1 else 0)
        |> List.fold ~init:0 ~f:( + )
      in
      (1, conv)
  | true, false, false, false ->
      let _, b, c, _ = diagonals in
      let tmp = [ b; c ] in
      let conv =
        List.map tmp ~f:(fun x -> if x then 1 else 0)
        |> List.fold ~init:0 ~f:( + )
      in
      (1, conv)
  | false, true, false, false ->
      let _, _, c, d = diagonals in
      let tmp = [ c; d ] in
      let conv =
        List.map tmp ~f:(fun x -> if x then 1 else 0)
        |> List.fold ~init:0 ~f:( + )
      in
      (1, conv)
  | false, false, true, false ->
      let a, _, _, d = diagonals in
      let tmp = [ a; d ] in
      let conv =
        List.map tmp ~f:(fun x -> if x then 1 else 0)
        |> List.fold ~init:0 ~f:( + )
      in
      (1, conv)
  | false, false, false, true ->
      let a, b, _, _ = diagonals in
      let tmp = [ a; b ] in
      let conv =
        List.map tmp ~f:(fun x -> if x then 1 else 0)
        |> List.fold ~init:0 ~f:( + )
      in
      (1, conv)
  | false, true, false, true | true, false, true, false -> (1, 0)
  | true, true, false, false ->
      let _, _, a, _ = diagonals in
      if a then (1, 2) else (1, 1)
  | false, true, true, false ->
      let _, _, _, a = diagonals in
      if a then (1, 2) else (1, 1)
  | false, false, true, true ->
      let a, _, _, _ = diagonals in
      if a then (1, 2) else (1, 1)
  | true, false, false, true ->
      let _, a, _, _ = diagonals in
      if a then (1, 2) else (1, 1)
  | false, true, true, true
  | true, false, true, true
  | true, true, false, true
  | true, true, true, false ->
      (1, 2)
  | true, true, true, true -> (1, 4)

let solve_part2 (input : string list) =
  let garden = char_lists input in
  let map = ref (Map.empty (module PairInt)) in

  for x = 0 to List.length garden - 1 do
    for y = 0 to List.length garden - 1 do
      let group = determine_group !map garden x y in
      let update = check_position_part2 garden x y in
      map := update_map !map group update
    done
  done;

  List.map (Map.data !map) ~f:(fun (x, y) -> x * y)
  |> List.fold ~init:0 ~f:( + ) |> Int.to_string
