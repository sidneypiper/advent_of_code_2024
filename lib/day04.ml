open List

let to_char_array s = List.init (String.length s) (String.get s)

let to_string l = String.of_seq (List.to_seq l)

let get_at x y input = try nth (nth input y) x with _ -> ' '

let get_words_part1 x y input = 
  let north = [get_at x y input; get_at x (y - 1) input; get_at x (y - 2) input; get_at x (y - 3) input] in
  let north_east = [get_at x y input; get_at (x + 1) (y - 1) input; get_at (x + 2) (y - 2) input; get_at (x + 3) (y - 3) input] in
  let east = [get_at x y input; get_at (x + 1) y input; get_at (x + 2) y input; get_at (x + 3) y input] in
  let south_east = [get_at x y input; get_at (x + 1) (y + 1) input; get_at (x + 2) (y + 2) input; get_at (x + 3) (y + 3) input] in
  let south = [get_at x y input; get_at x (y + 1) input; get_at x (y + 2) input; get_at x (y + 3) input] in
  let south_west = [get_at x y input; get_at (x - 1) (y + 1) input; get_at (x - 2) (y + 2) input; get_at (x - 3) (y + 3) input] in
  let west = [get_at x y input; get_at (x - 1) y input; get_at (x - 2) y input; get_at (x - 3) y input] in
  let north_west = [get_at x y input; get_at (x - 1) (y - 1) input; get_at (x - 2) (y - 2) input; get_at (x - 3) (y - 3) input] in
  List.map (to_string) [north; north_east; east; south_east; south; south_west; west; north_west]

let solve_part1 (input: string list) =
  let n = ref 0 in
  let char_array = List.map to_char_array input in
  for y = 0 to length char_array do
    for x = 0 to length (nth char_array 0) do
      if get_at x y char_array = 'X' then
        let words = get_words_part1 x y char_array in
        n := !n + List.fold_left (+) 0 (List.map (fun w -> if w = "XMAS" then 1 else 0) words);
    done
  done;
  string_of_int !n

(*
M.S   M.M   S.M   S.S
.A.   .A.   .A.   .A.
M.S   S.S   S.M   M.M
*)

let get_words_part2 x y input = 
  let p1 = [get_at x y input; get_at (x + 1) (y + 1) input; get_at (x + 2) (y + 2) input; get_at x (y + 2) input; get_at (x + 2) y input]  in
  let p2 = [get_at x y input; get_at (x + 1) (y + 1) input; get_at (x + 2) (y + 2) input; get_at (x + 2) y input; get_at x (y + 2) input] in
  let p3 = [get_at x y input; get_at (x - 1) (y - 1) input; get_at (x - 2) (y - 2) input; get_at x (y - 2) input; get_at (x - 2) y input] in
  let p4 = [get_at x y input; get_at (x - 1) (y - 1) input; get_at (x - 2) (y - 2) input; get_at (x - 2) y input; get_at x (y - 2) input] in
  List.map (to_string) [p1; p2; p3; p4]

let solve_part2 (input: string list) = 
  let n = ref 0 in
  let char_array = List.map to_char_array input in
  for y = 0 to length char_array do
    for x = 0 to length (nth char_array 0) do
      if get_at x y char_array = 'M' then
        let words = get_words_part2 x y char_array in
        n := !n + List.fold_left (+) 0 (List.map (fun w -> if w = "MASMS" then 1 else 0) words);
    done
  done;
  string_of_int !n