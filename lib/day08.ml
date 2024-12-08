type vector = int * int
let add ((x1, y1): vector) ((x2, y2): vector): vector = (x1 + x2, y1 + y2)

let sub ((x1, y1): vector) ((x2, y2): vector): vector = (x1 - x2, y1 - y2)

let scale ((x1, y1): vector) (s: int): vector = (x1 * s, y1 * s)

module VectorSet = Set.Make(struct
  type t = vector
  let compare (x1, y1) (x2, y2) =
    let c = compare x1 x2 in
    if c = 0 then compare y1 y2 else c
end)

module CharMap = Map.Make(Char)

let add_to_map map key vector =
  let updated_list =
    match CharMap.find_opt key map with
    | Some vectors -> vector :: vectors
    | None -> [vector]
  in
  CharMap.add key updated_list map

let parse_input (input: string list) =
  let map = ref CharMap.empty in
  for y = 0 to List.length input - 1 do
    let line = List.nth input y in
    for x = 0 to String.length line - 1 do
      let c = String.get line x in
      if c <> '.' then
        map := add_to_map !map c (x, y)
    done
  done;
  !map

let rec generate_pairs list =
  match list with
  | [] -> []
  | x :: xs -> List.map (fun y -> (x, y)) xs @ generate_pairs xs

let generate_all_pairs map =
  CharMap.fold
    (fun _ positions acc -> acc @ generate_pairs positions)
    map
    []

let calc_antinodes_part1 pair =
  match pair with
  | (v, u) ->
    let delta = sub v u in
    [add v delta; sub u delta]

let remove_duplicates (vectors: vector list) =
  let set = List.fold_left (fun acc v -> VectorSet.add v acc) VectorSet.empty vectors in
  VectorSet.elements set

let solve_part1 (input: string list) =
  let max = List.length input in
  let antenna_map = parse_input input in
  let pairs = generate_all_pairs antenna_map in
  let antinodes = 
    List.flatten (List.map calc_antinodes_part1 pairs)
    |> remove_duplicates
    |> List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < max && y < max) in
  List.length antinodes
  |> string_of_int

let calc_antinodes_part2 pair =
  match pair with
  | (v, u) ->
    let rec helper n antinodes =
      match n with
      | 100 -> antinodes
      | n ->
        let delta = scale (sub v u) n in
        helper (n+1) (add u delta :: sub v delta :: antinodes)
      in helper 1 []

let solve_part2 (input: string list) = 
  let max = List.length input in
  let antenna_map = parse_input input in
  let pairs = generate_all_pairs antenna_map in
  let antinodes = 
    List.flatten (List.map calc_antinodes_part2 pairs)
    |> remove_duplicates
    |> List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < max && y < max) in
  List.length antinodes
  |> string_of_int