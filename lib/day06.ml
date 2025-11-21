module PositionSet = Set.Make (struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match compare x1 x2 with 0 -> compare y1 y2 | cmp -> cmp
end)

module PositionDirectionSet = Set.Make (struct
  type t = int * int * int

  let compare (x1, y1, z1) (x2, y2, z2) =
    match compare x1 x2 with
    | 0 -> ( match compare y1 y2 with 0 -> compare z1 z2 | cmp -> cmp)
    | cmp -> cmp
end)

let to_char_array s = List.init (String.length s) (String.get s)
let get_char_at map x y = List.nth (List.nth map y) x

let set_char_at map x y new_char =
  let row = List.nth map y in
  let updated_row = List.mapi (fun i c -> if i = x then new_char else c) row in
  let updated_map =
    List.mapi (fun i r -> if i = y then updated_row else r) map
  in
  updated_map

let find_char_position map =
  let rec find_in_map map y =
    match map with
    | [] -> None
    | row :: rest -> (
        match List.find_index (fun x -> x = '^') row with
        | Some x -> Some (x, y)
        | None -> find_in_map rest (y + 1))
  in
  find_in_map map 0

let step map x y =
  let max_y = List.length map in
  let max_x = List.length (List.hd map) in
  let rec helper map x y d visited =
    if x > 0 && x < max_x - 1 && y > 0 && y < max_y - 1 then
      let visited = PositionSet.add (x, y) visited in
      match d with
      | 0 ->
          if get_char_at map x (y - 1) = '#' then helper map x y 1 visited
          else helper map x (y - 1) d visited
      | 1 ->
          if get_char_at map (x + 1) y = '#' then helper map x y 2 visited
          else helper map (x + 1) y d visited
      | 2 ->
          if get_char_at map x (y + 1) = '#' then helper map x y 3 visited
          else helper map x (y + 1) d visited
      | 3 ->
          if get_char_at map (x - 1) y = '#' then helper map x y 0 visited
          else helper map (x - 1) y d visited
      | _ -> visited
    else visited
  in
  helper map x y 0 PositionSet.empty

let solve_part1 (input : string list) =
  let map = List.map to_char_array input in
  match find_char_position map with
  | None -> ""
  | Some (x, y) -> string_of_int @@ PositionSet.cardinal @@ step map x y

let check_loop map x y =
  let max_y = List.length map in
  let max_x = List.length (List.hd map) in
  let rec helper map x y d visited =
    if PositionDirectionSet.mem (x, y, d) visited then 1
    else if x > 0 && x < max_x - 1 && y > 0 && y < max_y - 1 then
      let visited = PositionDirectionSet.add (x, y, d) visited in
      match d with
      | 0 ->
          if get_char_at map x (y - 1) = '#' then helper map x y 1 visited
          else helper map x (y - 1) d visited
      | 1 ->
          if get_char_at map (x + 1) y = '#' then helper map x y 2 visited
          else helper map (x + 1) y d visited
      | 2 ->
          if get_char_at map x (y + 1) = '#' then helper map x y 3 visited
          else helper map x (y + 1) d visited
      | 3 ->
          if get_char_at map (x - 1) y = '#' then helper map x y 0 visited
          else helper map (x - 1) y d visited
      | _ -> 0
    else 0
  in
  helper map x y 0 PositionDirectionSet.empty

let solve_part2 (input : string list) =
  let map = List.map to_char_array input in
  match find_char_position map with
  | None -> ""
  | Some (x, y) ->
      let possible_stones = PositionSet.to_list (step map x y) in
      string_of_int @@ ( + ) 1 @@ List.fold_left ( + ) 0
      @@ List.map
           (fun (nx, ny) -> check_loop (set_char_at map nx ny '#') x y)
           possible_stones
