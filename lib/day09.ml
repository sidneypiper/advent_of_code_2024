module IntMap = Map.Make (Int)

let to_int_list s =
  let offset = int_of_char '0' in
  List.init (String.length s) (fun i -> int_of_char s.[i] - offset)

let to_file_free_map lst =
  let rec helper lst n file free =
    match lst with
    | [] -> (file, free, (n - 1) / 2)
    | x :: xs ->
        if n mod 2 = 0 then helper xs (n + 1) (IntMap.add (n / 2) x file) free
        else helper xs (n + 1) file (IntMap.add ((n - 1) / 2) x free)
  in
  helper lst 0 IntMap.empty IntMap.empty

let subtract_one map key =
  IntMap.update key (function None -> None | Some v -> Some (v - 1)) map

let print_map map =
  IntMap.iter
    (fun key value -> Printf.printf "Key: %d, Value: %d\n" key value)
    map

let checksum_part1 file free max =
  let rec helper file free pos file_min file_max free_pos is_file acc =
    match is_file with
    | true ->
        if IntMap.find file_min file = 1 then
          helper file free (pos + 1) (file_min + 1) file_max free_pos false
            ((pos * file_min) + acc)
        else
          helper
            (subtract_one file file_min)
            free (pos + 1) file_min file_max free_pos true
            ((pos * file_min) + acc)
    | false ->
        if file_min > file_max then acc
        else if IntMap.find free_pos free = 1 && IntMap.find file_max file = 1
        then
          helper file free (pos + 1) file_min (file_max - 1) (free_pos + 1) true
            ((pos * file_max) + acc)
        else if IntMap.find free_pos free = 1 then
          helper
            (subtract_one file file_max)
            free (pos + 1) file_min file_max (free_pos + 1) true
            ((pos * file_max) + acc)
        else if IntMap.find free_pos free = 0 then
          helper file free pos file_min file_max (free_pos + 1) true acc
        else if IntMap.find file_max file = 1 then
          helper file
            (subtract_one free free_pos)
            (pos + 1) file_min (file_max - 1) free_pos false
            ((pos * file_max) + acc)
        else
          helper
            (subtract_one file file_max)
            (subtract_one free free_pos)
            (pos + 1) file_min file_max free_pos false
            ((pos * file_max) + acc)
  in
  helper file free 0 0 max 0 true 0

let solve_part1 (input : string list) =
  let file, free, max = List.hd input |> to_int_list |> to_file_free_map in
  checksum_part1 file free max |> string_of_int

let parse_input input =
  let rec helper chars file_id is_file result n =
    match chars with
    | [] -> (List.rev result, (n - 1) / 2)
    | x :: xs ->
        let size = int_of_char x - int_of_char '0' in
        let list = List.init size (fun _ -> if is_file then file_id else -1) in
        helper xs
          (if is_file then file_id + 1 else file_id)
          (not is_file) (list @ result) (n + 1)
  in
  helper (List.init (String.length input) (String.get input)) 0 true [] 0

let find_leftmost_free_block_of_size lst size_required =
  let rec helper lst n start size in_block =
    match lst with
    | [] when in_block && size >= size_required -> Some (start, start + size)
    | [] -> None
    | x :: xs when in_block ->
        if x = -1 then
          let new_size = size + 1 in
          if new_size >= size_required then Some (start, start + new_size)
          else helper xs (n + 1) start new_size true
        else helper xs (n + 1) 0 0 false
    | x :: xs ->
        if x = -1 then
          if size_required = 1 then Some (n, n + 1)
          else helper xs (n + 1) n 1 true
        else helper xs (n + 1) 0 0 false
  in
  helper lst 0 0 0 false

let find_block_by_id lst id =
  let rec helper lst n start size in_block =
    match lst with
    | [] when in_block -> (start, start + size)
    | [] -> (0, 0)
    | x :: xs when in_block ->
        if x = id then helper xs (n + 1) start (size + 1) true
        else (start, start + size)
    | x :: xs ->
        if x = id then helper xs (n + 1) n 1 true
        else helper xs (n + 1) 0 0 false
  in
  helper lst 0 0 0 false

let shift_block lst file_start file_end free_start free_end file_id =
  List.mapi
    (fun i x ->
      if i >= free_start && i < free_end then file_id
      else if i >= file_start && i < file_end then -1
      else x)
    lst

let print_list lst =
  Printf.printf "";
  List.iter
    (fun x -> if x >= 0 then Printf.printf "%d" x else print_char '.')
    lst;
  Printf.printf "\n"

let rec to_checksum_list lst n =
  let file_start, file_end = find_block_by_id lst n in
  let file_size = file_end - file_start in
  match find_leftmost_free_block_of_size lst file_size with
  | None -> to_checksum_list lst (n - 1)
  | Some (free_start, free_end) ->
      if n = 0 then lst
      else if file_start < free_start then to_checksum_list lst (n - 1)
      else
        to_checksum_list
          (shift_block lst file_start file_end free_start free_end n)
          (n - 1)

let solve_part2 (input : string list) =
  let input_list, n = parse_input (List.hd input) in
  let ckecksum_list = to_checksum_list input_list n in
  let map_list =
    List.mapi (fun i x -> if x = -1 then 0 else i * x) ckecksum_list
  in
  let checksum = List.fold_left ( + ) 0 map_list in
  string_of_int checksum
