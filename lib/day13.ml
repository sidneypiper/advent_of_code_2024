open Base

let parse_input (lines: string list): (float * float * float * float * float * float) list =
  let extract_button line =
    let parts = String.split ~on:' ' line in
    let x = String.chop_prefix_exn ~prefix:"X+" (List.nth_exn parts 2) |> String.chop_suffix_exn ~suffix:"," in
    let y = String.chop_prefix_exn ~prefix:"Y+" (List.last_exn parts) in
    (Float.of_string x, Float.of_string y)
  in
  let extract_prize line =
    let parts = String.split ~on:' ' line in
    let x = String.chop_prefix_exn ~prefix:"X=" (List.nth_exn parts 1) |> String.chop_suffix_exn ~suffix:"," in
    let y = String.chop_prefix_exn ~prefix:"Y=" (List.last_exn parts) in
    (Float.of_string x, Float.of_string y)
  in
  let rec parse_blocks acc = function
    | a :: b :: prize :: _ :: rest
    | a :: b :: prize :: rest ->
      let (ax, ay) = extract_button a in
      let (bx, by) = extract_button b in
      let (px, py) = extract_prize prize in
      parse_blocks ((ax, ay, bx, by, px, py) :: acc) rest
    | [] -> List.rev acc
    | _ -> failwith "Unexpected number of lines"
  in
  parse_blocks [] lines


(*
  B-Press = (X * Ay - Ax * Y) / (Bx * Ay - Ax * By) 
  A-Press = (X - B-Press * Bx) / Ax
*)

let calc_presses variables =
  let (ax, ay, bx, by, px, py) = variables in
  let b = (px *. ay -. ax *. py) /. (bx *. ay -. ax *. by) in
  let a = (px -. b *. bx) /. ax in
  if Float.is_integer a && Float.is_integer b then
    3 * Float.to_int a + Float.to_int b
  else
    0

let solve_part1 (input: string list) = 
  parse_input input
  |> List.map ~f:calc_presses
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string

let calc_presses variables =
  let (ax, ay, bx, by, px, py) = variables in
  let px = 10000000000000. +. px in
  let py = 10000000000000. +. py in
  let b = (px *. ay -. ax *. py) /. (bx *. ay -. ax *. by) in
  let a = (px -. b *. bx) /. ax in
  if Float.is_integer a && Float.is_integer b then
    3 * Float.to_int a + Float.to_int b
  else
    0

let solve_part2 (input: string list) =
  parse_input input
  |> List.map ~f:calc_presses
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string