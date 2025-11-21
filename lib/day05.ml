module IntMap = Map.Make (Int)

let split_on_empty_line lines =
  let rec helper lines acc1 acc2 in_second_list =
    match lines with
    | [] -> (List.rev acc1, List.rev acc2)
    | "" :: rest -> helper rest acc1 acc2 true
    | line :: rest ->
        if in_second_list then helper rest acc1 (line :: acc2) true
        else helper rest (line :: acc1) acc2 false
  in
  helper lines [] [] false

let parse_rules lines =
  let rec helper lines graph =
    match lines with
    | [] -> graph
    | line :: rest ->
        let num1, num2 = Scanf.sscanf line "%d|%d" (fun a b -> (a, b)) in
        let updated_graph =
          IntMap.update num2
            (function None -> Some [ num1 ] | Some lst -> Some (num1 :: lst))
            graph
        in
        let updated_graph =
          IntMap.update num1
            (function None -> Some [] | Some lst -> Some lst)
            updated_graph
        in
        helper rest updated_graph
  in
  helper lines IntMap.empty

let parse_updates lines =
  List.map
    (fun line -> List.map int_of_string (String.split_on_char ',' line))
    lines

let compare graph a b =
  if List.mem b (IntMap.find a graph) then -1
  else if List.mem a (IntMap.find b graph) then 1
  else 0

let sort_list rules lst =
  let compare_values x y = compare rules x y in
  List.rev @@ List.sort compare_values lst

let middle_element lst = List.nth lst @@ ((List.length lst - 1) / 2)

let solve_part1 (input : string list) =
  let graph_list, update_list = split_on_empty_line input in
  let rules = parse_rules graph_list in
  let update = parse_updates update_list in
  let valid_list =
    List.map
      (fun x -> if x = sort_list rules x then middle_element x else 0)
      update
  in
  string_of_int @@ List.fold_left ( + ) 0 valid_list

let solve_part2 (input : string list) =
  let graph_list, update_list = split_on_empty_line input in
  let rules = parse_rules graph_list in
  let update = parse_updates update_list in
  let valid_list =
    List.map
      (fun x ->
        let sorted = sort_list rules x in
        if x = sorted then 0 else middle_element sorted)
      update
  in
  string_of_int @@ List.fold_left ( + ) 0 valid_list
