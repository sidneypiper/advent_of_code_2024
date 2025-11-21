let get_bit num pos = (num lsr pos) land 1 = 1

let check_valid_part1 result numbers =
  let max = (1 lsl List.length numbers) - 1 in
  let rec helper result numbers n =
    if n > max then 0
    else
      let calc =
        List.fold_left
          (fun (i, acc) x -> (i + 1, if get_bit n i then acc * x else acc + x))
          (0, List.hd numbers)
          (List.tl numbers)
        |> snd
      in
      if result = calc then result else helper result numbers (n + 1)
  in
  helper result numbers 0

let extract_numbers line =
  line
  |> String.map (fun c -> if c = ':' then ' ' else c)
  |> String.split_on_char ' '
  |> List.filter (fun x -> x <> "")
  |> List.map int_of_string

let solve_part1 (input : string list) =
  let numbers = List.map extract_numbers input in
  string_of_int @@ List.fold_left ( + ) 0
  @@ List.map (fun x -> check_valid_part1 (List.hd x) (List.tl x)) numbers

let get_trit n pos = n / int_of_float (Float.pow 3.0 (float_of_int pos)) mod 3
let concat_numbers a b = int_of_string (string_of_int a ^ string_of_int b)

let check_valid_part2 result numbers =
  let max =
    int_of_float (Float.pow 3.0 (float_of_int (List.length numbers))) - 1
  in
  let rec helper result numbers n =
    if n > max then 0
    else
      let calc =
        List.fold_left
          (fun (i, acc) x ->
            let operator = get_trit n i in
            let acc =
              match operator with
              | 0 -> acc + x
              | 1 -> acc * x
              | 2 -> concat_numbers acc x
              | _ -> acc
            in
            (i + 1, acc))
          (0, List.hd numbers)
          (List.tl numbers)
        |> snd
      in
      if result = calc then result else helper result numbers (n + 1)
  in
  helper result numbers 0

let solve_part2 (input : string list) =
  let numbers = List.map extract_numbers input in
  string_of_int @@ List.fold_left ( + ) 0
  @@ List.map (fun x -> check_valid_part2 (List.hd x) (List.tl x)) numbers
