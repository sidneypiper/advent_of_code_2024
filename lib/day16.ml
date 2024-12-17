open Base

module PriorityQueue = struct
  type t = ((int * int) * int) list
  let empty = []
  let is_empty queue = List.is_empty queue
  let rec add queue elem priority =
    match queue with
    | [] -> [(elem, priority)]
    | (x, p) :: xs ->
      if priority <= p then (elem, priority) :: (x, p) :: xs
      else (x, p) :: add xs elem priority
  let pop = function
    | [] -> None
    | (x, _) :: xs -> Some (x, xs)
end

type direction = North | East | South | West

type state = {
  x : int;
  y : int;
  dir : direction;
  cost : int;
}

let turn_left = function
  | North -> West
  | West -> South
  | South -> East
  | East -> North

let turn_right = function
  | North -> East
  | East -> South
  | South -> West
  | West -> North

let move_forward (x, y) = function
  | North -> (x - 1, y)
  | East -> (x, y + 1)
  | South -> (x + 1, y)
  | West -> (x, y - 1)

let find_position maze c =
  let pos = ref (-1, -1) in
  Grid.iter (fun (y, x) cur -> if Char.(cur = c) then pos := (x, y)) maze;
  !pos

let solve_maze maze =
  let pq = PriorityQueue.empty in
  let visited = Hashtbl.Poly.create () in
  let (sx, sy) = find_position maze 'S' in
  let (gx, gy) = find_position maze 'E' in
  let start_state = { x = sx; y = sy; dir = East; cost = 0 } in
  let pq = PriorityQueue.add pq start_state 0 in

  let rec loop pq =
    match PriorityQueue.pop pq with
    | None -> -1
    | Some ({ x; y; dir; cost }, pq) ->
        if x = gx && y = gy then cost
        else 
          match Hashtbl.find visited (x, y, dir) with
          | Some prev_cost when prev_cost <= cost -> loop pq
          | _ ->
              Hashtbl.set visited ~key:(x, y, dir) ~data:cost;
  
              let (fx, fy) = move_forward (x, y) dir in
              let neighbors = [
                { x = fx; y = fy; dir; cost = cost + 1 };
                { x; y; dir = turn_left dir; cost = cost + 1000 };
                { x; y; dir = turn_right dir; cost = cost + 1000 };
                ]
              in

              let pq =
                List.fold neighbors ~init:pq ~f:(
                  fun pq { x = nx; y = ny; dir = ndir; cost = ncost } ->
                    if Char.(Grid.get maze (nx, ny) = '#') then pq
                    else PriorityQueue.add pq { x = nx; y = ny; dir = ndir; cost = ncost } ncost
                )
                
              in loop pq 
  in loop pq

let parse_maze input =
  let height = List.length input in
  let width = String.length (List.hd_exn input) in
  Grid.init height width (fun (row, col) ->
      String.get (List.nth_exn input row) col)

let print_maze (grid : char Grid.t) : unit =
  let width = Grid.width grid - 1 in
  Grid.iter (fun (_, x) c -> if x = width then Stdio.printf "%c\n" c else Stdio.printf "%c" c) grid


(* For my input this solution was of by 1000  *)
let solve_part1 (input: string list) =
    parse_maze input
    |> solve_maze
    |> Int.to_string 

let solve_part2 (_input: string list) = ""