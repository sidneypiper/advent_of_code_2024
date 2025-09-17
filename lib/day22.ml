open Base

let parse_input lines = 
  List.map lines ~f:Int.of_string

let next_number x =
  let x = (Int.bit_xor (x * 64) x) % 16777216 in
  let x = Int.bit_xor (x / 32) x % 16777216 in
  Int.bit_xor (x * 2048) x % 16777216

let secret_numbers seed n =
  let rec aux acc current count =
    if count = 0 then List.rev acc
    else aux (current :: acc) (next_number current) (count - 1)
  in
  aux [] seed n

let calc_prices numbers =
  List.map numbers ~f:(fun x -> x % 10)

let rec calc_differences = function
  | [] | [_] -> []
  | x :: (y :: _ as tl) -> (y - x) :: calc_differences tl

let rec create_patterns = function
    | a :: b :: c :: d :: tl -> (a, b, c, d) :: create_patterns (b :: c :: d :: tl)
    | _ -> []

let patterns_with_price patterns prices =
  List.zip_exn patterns (List.drop prices 4)

let create_pattern_price_set lst =
  let hashmap = Hashtbl.Poly.create () in
  List.iter lst ~f:(fun (pattern, price) ->
    match Hashtbl.find hashmap pattern with
    | Some _ -> ()
    | None -> Hashtbl.set hashmap ~key:pattern ~data:price);
  Hashtbl.to_alist hashmap

let solve_part1 input =
  let parsed = parse_input input in
  List.map parsed ~f:(Fn.apply_n_times ~n:2000 next_number)
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string

let solve_part2 input =
  let parsed = parse_input input in
  let tbl = Hashtbl.Poly.create () in
  List.iter parsed ~f:(fun seed ->
    let prices = secret_numbers seed 2000 |> calc_prices in
    let patterns = calc_differences prices |> create_patterns in
    let set = patterns_with_price patterns prices |> create_pattern_price_set in
    List.iter set ~f:(fun (pattern, price) ->
      match Hashtbl.find tbl pattern with
      | Some x -> Hashtbl.set tbl ~key:pattern ~data:(price + x)
      | None -> Hashtbl.set tbl ~key:pattern ~data:price
    )
  );
  Hashtbl.fold tbl ~init:0 ~f:(fun ~key:_ ~data acc -> Int.max acc data)
  |> Int.to_string