open Str

let extract_mul_occurrences input =
  let regex = regexp {|mul(\([0-9]+\),\([0-9]+\))|} in
  let rec collect_matches acc pos =
    try
      let _ = search_forward regex input pos in
      let x = matched_group 1 input in
      let y = matched_group 2 input in
      collect_matches ((int_of_string x, int_of_string y) :: acc) (match_end ())
    with Not_found -> acc
  in
  collect_matches [] 0

let extract_do_mul_occurrences input =
  let regex = regexp {|mul(\([0-9]+\),\([0-9]+\))|} in
  let rec collect_matches acc pos =
    try
      let mul_pos = search_forward regex input pos in
      let x = matched_group 1 input in
      let y = matched_group 2 input in
      let ende = match_end () in
      let do_pos =
        try search_backward (regexp {|\(do()\)|}) input mul_pos
        with Not_found -> 0
      in
      let dont_pos =
        try search_backward (regexp {|\(don\'t()\)|}) input mul_pos
        with Not_found -> max_int
      in
      if do_pos > dont_pos || dont_pos = max_int then
        collect_matches ((int_of_string x, int_of_string y) :: acc) ende
      else collect_matches acc ende
    with Not_found -> acc
  in
  collect_matches [] 0

let solve_part1 (input : string list) =
  let input_string = String.concat "" input in
  let to_mul_list = extract_mul_occurrences input_string in
  let mul_list = List.map (fun (x, y) -> x * y) to_mul_list in
  string_of_int @@ List.fold_left ( + ) 0 mul_list

let solve_part2 (input : string list) =
  let input_string = String.concat "" input in
  let to_mul_list = extract_do_mul_occurrences input_string in
  let mul_list = List.map (fun (x, y) -> x * y) to_mul_list in
  string_of_int @@ List.fold_left ( + ) 0 mul_list
