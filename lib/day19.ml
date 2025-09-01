open Base

let parse_input input =
  match input with
  | towels :: _ :: patterns ->
    let towels = String.substr_replace_all ~pattern:" " ~with_:"" towels |> String.split ~on:',' in
    (towels, patterns)
  | _ -> failwith "Invalid input format"

let memo = Hashtbl.Poly.create ()

let rec count_possibilities towels pattern =
  match Hashtbl.find memo pattern with
  | Some result -> result
  | None ->
    let result =
      if String.is_empty pattern then 1
      else
        let chopped = List.filter_map towels ~f:(fun towel -> String.chop_prefix ~prefix:towel pattern) in
        List.sum (module Int) chopped ~f:(count_possibilities towels)
    in
    Hashtbl.set memo ~key:pattern ~data:result;
    result

let solve_part1 (input: string list) =
  let towels, patterns = parse_input input in
  List.filter patterns ~f:(fun(pattern) -> count_possibilities towels pattern  >= 1)
  |> List.length
  |> Int.to_string

let solve_part2 (input: string list) = 
  let towels, patterns = parse_input input in
  List.sum (module Int) patterns ~f:(fun(pattern) -> count_possibilities towels pattern)
  |> Int.to_string