open Base

let parse_input input = 
  List.map ~f:(String.append "A") input

let get_pairs code =
  let rec zip xs ys acc =
    match (xs, ys) with
    | (x::xs', y::ys') -> zip xs' ys' ((x, y) :: acc)
    | _ -> List.rev acc
  in
  match String.to_list code with
  | head :: tail -> zip (head :: tail) tail []
  | _ -> []

let num_pad = function
  | '0' -> (1, 0)
  | 'A' -> (2, 0)
  | '1' -> (0, 1)
  | '2' -> (1, 1)
  | '3' -> (2, 1)
  | '4' -> (0, 2)
  | '5' -> (1, 2)
  | '6' -> (2, 2)
  | '7' -> (0, 3)
  | '8' -> (1, 3)
  | '9' -> (2, 3)
  | _ -> failwith "This key does not exist on the numeric keypad."

let dir_pad = function
  | '<' -> (0, 0)
  | 'v' -> (1, 0)
  | '>' -> (2, 0)
  | '^' -> (1, 1)
  | 'A' -> (2, 1)
  | _ -> failwith "This key does not exist on the directional keypad."

let repeat c n =
  String.make n c

let path_on_num_pad (a, b) =
  let x1, y1 = num_pad a in
  let x2, y2 = num_pad b in
  let dx, dy = x2 - x1, y2 - y1 in
  if x1 = 0 && y2 = 0 then
    String.concat [repeat '>' (Int.abs dx); repeat 'v' (Int.abs dy); "A"]
  else if y1 = 0 && x2 = 0 then
    String.concat [repeat '^' (Int.abs dx); repeat '<' (Int.abs dy); "A"]
  else if dx = 0 && dy = 0 then
    "A"
  else if dx > 0 && dy = 0 then
    String.concat [repeat '>' (Int.abs dx); "A"]
  else if dx < 0 && dy = 0 then
    String.concat [repeat '<' (Int.abs dx); "A"]
  else if dx = 0 && dy > 0 then
    String.concat [repeat '^' (Int.abs dy); "A"]
  else if dx = 0 && dy < 0 then
    String.concat [repeat 'v' (Int.abs dy); "A"]
  else if dx > 0 && dy > 0 then
    String.concat [repeat '^' (Int.abs dy); repeat '>' (Int.abs dx); "A"]
  else if dx < 0 && dy > 0 then
    String.concat [repeat '<' (Int.abs dx); repeat '^' (Int.abs dy); "A"]
  else if dx > 0 && dy < 0 then
    String.concat [repeat 'v' (Int.abs dy); repeat '>' (Int.abs dx); "A"]
  else
    String.concat [repeat '<' (Int.abs dx); repeat 'v' (Int.abs dy); "A"]


let path_on_dir_pad (a, b) =
  let x1, y1 = dir_pad a in
  let x2, y2 = dir_pad b in
  let dx, dy = x2 - x1, y2 - y1 in
  if x1 = 0 && y2 = 1 then
    String.concat [repeat '>' (Int.abs dx); repeat '^' (Int.abs dy); "A"]
  else if y1 = 1 && x2 = 0 then
    String.concat [repeat 'v' (Int.abs dy); repeat '<' (Int.abs dx); "A"]
  else if dx = 0 && dy = 0 then
    "A"
  else if dx > 0 && dy = 0 then
    String.concat [repeat '>' (Int.abs dx); "A"]
  else if dx < 0 && dy = 0 then
    String.concat [repeat '<' (Int.abs dx); "A"]
  else if dx = 0 && dy > 0 then
    String.concat [repeat '^' (Int.abs dy); "A"]
  else if dx = 0 && dy < 0 then
    String.concat [repeat 'v' (Int.abs dy); "A"]
  else if dx > 0 && dy > 0 then
    String.concat [repeat '^' (Int.abs dy); repeat '>' (Int.abs dx); "A"]
  else if dx < 0 && dy > 0 then
    String.concat [repeat '<' (Int.abs dx); repeat '^' (Int.abs dy); "A"]
  else if dx > 0 && dy < 0 then
    String.concat [repeat 'v' (Int.abs dy); repeat '>' (Int.abs dx); "A"]
  else
    String.concat [repeat '<' (Int.abs dx); repeat 'v' (Int.abs dy); "A"]

let process_line code n =
  let memo = Hashtbl.Poly.create () in
  let rec aux code n =
    Hashtbl.find_or_add memo (code, n) ~default:(fun () ->
      if n = 0 then String.length code
      else List.fold ~init:0 ~f:(+) (List.map ~f:(fun (a, b) -> aux (path_on_dir_pad (a, b)) (n - 1)) (get_pairs ("A" ^ code)))
    )
  in
  let step1 = String.concat @@ List.map ~f:path_on_num_pad @@ get_pairs code in
  aux step1 n

let get_numeric_part code =
  let len = String.length code in
  let num_str = String.sub code ~pos:1 ~len:(len - 2) in
  Int.of_string num_str

let solve input n =
  let parsed = parse_input input in
  let results = List.map parsed ~f:(fun code -> process_line code n) in
  let numeric = List.map parsed ~f:get_numeric_part in
  let zipped = List.zip_exn numeric results in
  List.map zipped ~f:(fun (num, len) -> num * len) 
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string

let solve_part1 input =
  solve input 2

let solve_part2 input =
  solve input 25