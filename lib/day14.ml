open Base

let parse_input (input: string list): (int * int * int * int) list =
  let extract_numbers line = 
    let parts = String.split ~on:' ' line in
    let p = String.chop_prefix_exn ~prefix:"p=" (List.nth_exn parts 0) |> String.split ~on:',' in
    let v = String.chop_prefix_exn ~prefix:"v=" (List.nth_exn parts 1) |> String.split ~on:',' in
    let px, py, vx, vy = List.nth_exn p 0, List.nth_exn p 1, List.nth_exn v 0, List.nth_exn v 1 in
    (Int.of_string px, Int.of_string py, Int.of_string vx, Int.of_string vy) 
  in
  let rec helper input acc =
    match input with
  | [] -> List.rev acc
  | x :: xs -> helper xs (extract_numbers x :: acc)
  in helper input []

let calc_quadrant (robot: int * int * int * int) (seconds: int): int =
  let px, py, vx, vy = robot in
  let x, y = (px + (seconds * vx)) % 101, (py + (seconds * vy)) % 103 in
  let horizontally, vertically = (101 - 1) / 2, (103- 1) / 2 in
  if x = horizontally || y = vertically then 0
  else if x < horizontally && y < vertically then 1
  else if x < horizontally && y > vertically then 2
  else if x > horizontally && y < vertically then 3
  else 4

let solve_part1 (input: string list) =
  let quadrants = parse_input input
  |> List.map ~f:(fun x -> calc_quadrant x 100) in
  let a = List.count ~f:(fun x -> x = 1) quadrants in
  let b = List.count ~f:(fun x -> x = 2) quadrants in
  let c = List.count ~f:(fun x -> x = 3) quadrants in
  let d = List.count ~f:(fun x -> x = 4) quadrants in

  a * b * c * d
  |> Int.to_string


let find_tree (robots: (int * int * int * int) list): int =
  let rec helper robots seconds =
    let quadrants = List.map ~f:(fun x -> calc_quadrant x seconds) robots in
    let a = List.count ~f:(fun x -> x = 1) quadrants in
    let b = List.count ~f:(fun x -> x = 2) quadrants in
    let c = List.count ~f:(fun x -> x = 3) quadrants in
    let d = List.count ~f:(fun x -> x = 4) quadrants in
    if a > b + c + d || b > a + c + d || c > a + b + d || d > a + b + c then
      seconds
    else
      helper robots (seconds + 1)
    in helper robots 0


let solve_part2 (input: string list) =
  parse_input input
  |> find_tree 
  |> Int.to_string