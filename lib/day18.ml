open Base

let x = 70
let dimx = x + 1
let dimy = x + 1
let start = (0, 0)
let goal = (x, x)

let parse_input input =
  List.map
    ~f:(fun line ->
      match String.split ~on:',' line with
      | [ x; y ] -> (Int.of_string x, Int.of_string y)
      | _ -> failwith "Invalid input format")
    input

let fill_memory memory data =
  List.iter data ~f:(fun (x, y) -> memory.(y).(x) <- 1)

let find_shortest_path memory =
  let start = start in
  let goal = goal in
  let visited = Hashtbl.Poly.create () in
  let queue = Queue.create () in
  Queue.enqueue queue (start, 0);
  let rec bfs () =
    match Queue.dequeue queue with
    | None -> None
    | Some ((x, y), dist) ->
        if x = fst goal && y = snd goal then Some dist
        else if Hashtbl.mem visited (x, y) then bfs ()
        else (
          Hashtbl.set visited ~key:(x, y) ~data:true;
          let neighbors = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ] in
          List.iter neighbors ~f:(fun (nx, ny) ->
              if
                nx >= 0 && nx < dimx && ny >= 0 && ny < dimy
                && memory.(ny).(nx) = 0
              then Queue.enqueue queue ((nx, ny), dist + 1));
          bfs ())
  in
  bfs ()

let print_memory memory =
  Array.iter memory ~f:(fun row ->
      Array.iter row ~f:(fun cell -> Stdio.printf "%d " cell);
      Stdio.printf "\n")

let solve_part1 (input : string list) =
  let memory = Array.make_matrix ~dimx ~dimy 0 in
  let data = parse_input input in
  let data = List.take data 1024 in
  fill_memory memory data;
  let cost = find_shortest_path memory in
  match cost with None -> "Not found" | Some c -> Int.to_string c

let rec binary_search data low high =
  let memory = Array.make_matrix ~dimx ~dimy 0 in
  let data' = List.take data high in
  fill_memory memory data';
  let cost = find_shortest_path memory in
  let mid = (high - low) / 2 in
  if mid = 0 then low
  else
    match cost with
    | None -> binary_search data low (high - mid)
    | Some _ -> binary_search data high (high + mid)

let solve_part2 (input : string list) =
  let data = parse_input input in
  let length = List.length data in
  let break = binary_search data 0 length in
  List.nth_exn data break |> fun (x, y) ->
  Int.to_string x ^ "," ^ Int.to_string y
