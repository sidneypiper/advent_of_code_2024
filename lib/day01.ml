let parse_input lines =
  let rec helper lines left right =
    match lines with
    | [] -> left, right
    | line :: rest ->
        let num1, num2 = Scanf.sscanf line "%d %d" (fun a b -> a, b) in
        helper rest (num1 :: left) (num2 :: right)
  in
  helper lines [] []

let solve_part1 input = 
  let left, right = parse_input input in
  let left_sorted, right_sorted = List.sort compare left, List.sort compare right in
  let distances = List.map2 (fun a b -> abs(a - b)) left_sorted  right_sorted in
  string_of_int (List.fold_left (+) 0 distances)

let rec occurrences e = function
| [] -> 0
| x :: xs -> if e = x then 1 + occurrences e xs else occurrences e xs

let solve_part2 input = 
  let left, right = parse_input input in
  let similarities = List.map (fun x -> x * (occurrences x right)) left in
  string_of_int (List.fold_left (+) 0 similarities)