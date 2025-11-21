module IntMap = Map.Make (Int)

let parse_input_part1 input =
  String.split_on_char ' ' input |> List.map int_of_string

let step_part1 stones =
  let rec helper stones acc =
    match stones with
    | [] -> acc
    | x :: xs ->
        if x = 0 then helper xs (1 :: acc)
        else
          let s = string_of_int x in
          let len = String.length s in
          if len mod 2 = 0 then
            let left = String.sub s 0 (len / 2) in
            let right = String.sub s (len / 2) (len / 2) in
            helper xs (int_of_string left :: int_of_string right :: acc)
          else helper xs ((x * 2024) :: acc)
  in
  helper stones []

let solve_part1 (input : string list) =
  let stones = ref [] in
  stones := parse_input_part1 (List.hd input);
  for _ = 1 to 25 do
    stones := step_part1 !stones
  done;
  string_of_int (List.length !stones)

let parse_input_part2 input =
  let stones = parse_input_part1 input in
  let rec helper stones map =
    match stones with [] -> map | x :: xs -> helper xs (IntMap.add x 1 map)
  in
  helper stones IntMap.empty

let step_part2 stones =
  let apply_rules key value acc =
    if key = 0 then
      match IntMap.find_opt 1 acc with
      | None -> IntMap.add 1 value acc
      | Some x -> IntMap.add 1 (value + x) acc
    else
      let s = string_of_int key in
      let len = String.length s in
      if len mod 2 = 0 then
        let left = String.sub s 0 (len / 2) |> int_of_string in
        let right = String.sub s (len / 2) (len / 2) |> int_of_string in
        let acc =
          match IntMap.find_opt left acc with
          | None -> IntMap.add left value acc
          | Some x -> IntMap.add left (value + x) acc
        in
        match IntMap.find_opt right acc with
        | None -> IntMap.add right value acc
        | Some x -> IntMap.add right (value + x) acc
      else
        match IntMap.find_opt (key * 2024) acc with
        | None -> IntMap.add (key * 2024) value acc
        | Some x -> IntMap.add (key * 2024) (value + x) acc
  in
  IntMap.fold
    (fun key value acc -> apply_rules key value acc)
    stones IntMap.empty

let solve_part2 (input : string list) =
  let stones = ref IntMap.empty in
  stones := parse_input_part2 (List.hd input);
  for _ = 1 to 75 do
    stones := step_part2 !stones
  done;
  IntMap.fold (fun _ value acc -> acc + value) !stones 0 |> string_of_int
