let parse_input lines = 
  let rec helper lines numbers =
    match lines with
    | [] -> numbers
    | x :: xs ->
      let split = String.split_on_char ' ' x in
        helper xs (List.map int_of_string split :: numbers)
    in
    helper lines []

let rec for_all f = function
| [] | [_] -> true
| x :: y :: xs -> if f x y then for_all f (y :: xs) else false
let is_asc (numbers: int list) = for_all (fun x y -> x < y) numbers

let is_desc (numbers: int list) = for_all (fun x y -> x > y) numbers

let is_save_diff (numbers: int list) = for_all (fun x y -> abs(x - y) > 0 && abs(x - y) < 4) numbers

let is_save_layer_part1 layer = is_save_diff layer && (is_asc layer || is_desc layer)

let solve f numbers = 
  let safe_list = List.map f numbers in
  string_of_int @@ List.fold_left (+) (0) (List.map (fun x -> if x then 1 else 0) safe_list)

let solve_part1 (input: string list) =
  let numbers = parse_input input in
  solve is_save_layer_part1 numbers

let remove_one lst =
  let rec aux acc prefix = function
    | [] -> List.rev acc
    | x :: xs -> aux ((prefix @ xs) :: acc) (prefix @ [x]) xs
  in
  aux [] [] lst
  
let is_save_layer_part2 layer =
  let new_layers = remove_one layer in
  List.fold_left (||) false (List.map is_save_layer_part1 new_layers)

let solve_part2 (input: string list) = 
  let numbers = parse_input input in
  solve is_save_layer_part2 numbers