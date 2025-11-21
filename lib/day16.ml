open Base

type direction = North | East | South | West
type state = { x : int; y : int; dir : direction; cost : int }

module PriorityQueue = struct
  type t = (state * int) list

  let empty = []
  let is_empty queue = List.is_empty queue

  let rec add queue elem priority =
    match queue with
    | [] -> [ (elem, priority) ]
    | (x, p) :: xs ->
        if priority <= p then (elem, priority) :: (x, p) :: xs
        else (x, p) :: add xs elem priority

  let pop = function [] -> None | (x, _) :: xs -> Some (x, xs)
end

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

let opposite_direction = function
  | North -> South
  | South -> North
  | East -> West
  | West -> East

let move_forward (x, y) = function
  | North -> (x - 1, y)
  | East -> (x, y + 1)
  | South -> (x + 1, y)
  | West -> (x, y - 1)

let move_left (x, y) dir = move_forward (x, y) (turn_left dir)
let move_right (x, y) dir = move_forward (x, y) (turn_right dir)

let find_position maze c =
  let pos = ref (-1, -1) in
  Grid.iter (fun (x, y) cur -> if Char.(cur = c) then pos := (x, y)) maze;
  !pos

let calc_costs maze start dir =
  let pq = PriorityQueue.empty in
  let visited = Hashtbl.Poly.create () in
  let sx, sy = start in
  let start_state = { x = sx; y = sy; dir; cost = 0 } in
  let pq = PriorityQueue.add pq start_state 0 in

  let rec loop pq =
    match PriorityQueue.pop pq with
    | None -> ()
    | Some ({ x; y; dir; cost }, pq) -> (
        match Hashtbl.find visited (x, y) with
        | Some (prev_cost, _) when prev_cost <= cost -> loop pq
        | _ ->
            Hashtbl.set visited ~key:(x, y) ~data:(cost, dir);

            let fx, fy = move_forward (x, y) dir in
            let lx, ly = move_left (x, y) dir in
            let rx, ry = move_right (x, y) dir in
            let neighbors =
              [
                { x = fx; y = fy; dir; cost = cost + 1 };
                { x = lx; y = ly; dir = turn_left dir; cost = cost + 1000 + 1 };
                { x = rx; y = ry; dir = turn_right dir; cost = cost + 1000 + 1 };
              ]
            in

            let pq =
              List.fold neighbors ~init:pq
                ~f:(fun pq { x = nx; y = ny; dir = ndir; cost = ncost } ->
                  if Char.(Grid.get maze (nx, ny) = '#') then pq
                  else
                    PriorityQueue.add pq
                      { x = nx; y = ny; dir = ndir; cost = ncost }
                      ncost)
            in
            loop pq)
  in
  loop pq;
  visited

let parse_maze input =
  let height = List.length input in
  let width = String.length (List.hd_exn input) in
  Grid.init height width (fun (row, col) ->
      String.get (List.nth_exn input row) col)

let print_maze (grid : char Grid.t) : unit =
  let width = Grid.width grid - 1 in
  Grid.iter
    (fun (_, x) c ->
      if x = width then Stdio.printf "%c\n" c else Stdio.printf "%c" c)
    grid

let solve_part1 (input : string list) =
  let maze = parse_maze input in
  let start = find_position maze 'S' in
  let costs = calc_costs maze start East in
  let ende = find_position maze 'E' in
  match Hashtbl.find costs ende with
  | Some (c, _) -> Int.to_string c
  | None -> "-1"

let solve_part2 (input : string list) =
  let maze = parse_maze input in
  let start = find_position maze 'S' in
  let ende = find_position maze 'E' in

  let costs_s = calc_costs maze start East in
  let cost_part1 =
    match Hashtbl.find costs_s ende with Some (c, _) -> c | None -> -1
  in

  let dir_to_e =
    match Hashtbl.find costs_s ende with
    | Some (_, dir) -> opposite_direction dir
    | None -> East
  in

  let costs_e = calc_costs maze ende dir_to_e in

  let count =
    Hashtbl.fold costs_s ~init:0 ~f:(fun ~key:(x, y) ~data:(cost_s, _) acc ->
        match Hashtbl.find costs_e (x, y) with
        | Some (cost_e, _)
          when cost_s + cost_e = cost_part1
               || cost_s + cost_e + 1000 = cost_part1 ->
            acc + 1
        | _ -> acc)
  in
  Int.to_string count
