open Base

let parse_input input =
  Array.of_list (List.map ~f:String.to_array input)

let find_position maze c =
  let pos = ref None in
  Array.iteri maze ~f:(fun y row ->
    Array.iteri row ~f:(fun x ch ->
      if Char.equal ch c then pos := Some (x, y)
    )
  );
  match !pos with
  | Some p -> p
  | None -> failwith (Printf.sprintf "Character %c not found in maze" c)

let find_shortest_path maze =
  let start = find_position maze 'S' in
  let goal = find_position maze 'E' in
  let visited = Hashtbl.Poly.create () in
  let queue = Queue.create () in
  Queue.enqueue queue (start, 0);
  let rec bfs path =
    match Queue.dequeue queue with
    | None -> None
    | Some ((x, y), dist) ->
      if (x = fst goal && y = snd goal) then Some (((x, y), dist) :: path)
      else (
        Hashtbl.set visited ~key:(x, y) ~data:true;

        let neighbors = List.filter ~f:(fun (nx, ny) ->
          not (Char.equal maze.(ny).(nx) '#') && not (Hashtbl.mem visited (nx, ny))
        ) [(x-1, y); (x+1, y); (x, y-1); (x, y+1)] in

        Queue.enqueue queue ((List.hd_exn neighbors), dist + 1);

        bfs (((x, y), dist) :: path)
      )
  in
  bfs []

let manhattan_distance (x1, y1) (x2, y2) =
  abs (x1 - x2) + abs (y1 - y2)

let solve_part1 input =
  let maze = parse_input input in
  match find_shortest_path maze with
  | Some path -> 
    let rec aux acc = function
    | [] -> acc
    | ((x, y), dist) :: rest ->
      List.fold rest ~init:acc ~f:(fun acc ((nx, ny), d) -> 
        if manhattan_distance (x, y) (nx, ny) = 2 && dist - d - 2 >= 100 then acc + 1 else acc)
      |> fun acc -> aux acc rest
    in
    aux 0 path
    |> Int.to_string
  | None -> "No path found"

let solve_part2 input = 
  let maze = parse_input input in
  match find_shortest_path maze with
  | Some path -> 
    let rec aux acc = function
    | [] -> acc
    | ((x, y), dist) :: rest ->
      List.fold rest ~init:acc ~f:(fun acc ((nx, ny), d) ->
        let md = manhattan_distance (x, y) (nx, ny) in
        if md <= 20 && dist - d - md >= 100 then acc + 1 else acc)
      |> fun acc -> aux acc rest
    in
    aux 0 path
    |> Int.to_string
  | None -> "No path found"