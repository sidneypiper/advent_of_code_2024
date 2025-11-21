let parse_to_2d_list (input : string list) : int list list =
  let parse_row (row : string) : int list =
    List.init (String.length row) (fun i ->
        int_of_char row.[i] - int_of_char '0')
  in
  List.map parse_row input

let reachable_9 (graph : int list list) (start_x : int) (start_y : int) : int =
  let visited = Hashtbl.create 100 in

  let rec dfs x y target_value reachable_count =
    try
      if Hashtbl.mem visited (x, y) then reachable_count
      else
        let current_value = List.nth (List.nth graph x) y in
        if current_value <> target_value then reachable_count
        else (
          Hashtbl.add visited (x, y) true;
          let new_count =
            if target_value = 9 then reachable_count + 1 else reachable_count
          in
          let neighbors = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ] in
          List.fold_left
            (fun acc (nx, ny) -> dfs nx ny (target_value + 1) acc)
            new_count neighbors)
    with _ -> reachable_count
  in
  dfs start_x start_y 0 0

let solve_part1 (input : string list) =
  let graph = parse_to_2d_list input in
  let n = ref 0 in
  for x = 0 to List.length graph - 1 do
    for y = 0 to List.length graph - 1 do
      if List.nth (List.nth graph x) y = 0 then n := !n + reachable_9 graph x y
    done
  done;
  string_of_int !n

let count_paths_to_9 (graph : int list list) (start_x : int) (start_y : int) :
    int =
  let rec dfs x y target_value =
    try
      let current_value = List.nth (List.nth graph x) y in
      if current_value <> target_value then 0
      else if target_value = 9 then 1
      else
        let neighbors = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ] in
        List.fold_left
          (fun acc (nx, ny) -> acc + dfs nx ny (target_value + 1))
          0 neighbors
    with _ -> 0
  in
  dfs start_x start_y 0

let solve_part2 (input : string list) =
  let graph = parse_to_2d_list input in
  let n = ref 0 in
  for x = 0 to List.length graph - 1 do
    for y = 0 to List.length graph - 1 do
      if List.nth (List.nth graph x) y = 0 then
        n := !n + count_paths_to_9 graph x y
    done
  done;
  string_of_int !n
