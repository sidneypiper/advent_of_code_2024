open Base

let parse_input (input : string list) =
  let get_reg_value line =
    match String.split_on_chars ~on:[ ':' ] line with
    | [ _; v ] -> Int.of_string (String.strip v)
    | _ -> failwith "Invalid register line"
  in
  let a = get_reg_value (List.nth_exn input 0) in
  let b = get_reg_value (List.nth_exn input 1) in
  let c = get_reg_value (List.nth_exn input 2) in
  let prog_line = List.nth_exn input 4 in
  let p =
    prog_line |> String.split ~on:':' |> List.rev |> List.hd_exn |> String.strip
    |> String.split ~on:','
    |> List.map ~f:(fun s -> Int.of_string (String.strip s))
  in
  (a, b, c, p)

let combo_operand x a b c =
  match x with
  | 0 | 1 | 2 | 3 -> x
  | 4 -> a
  | 5 -> b
  | 6 -> c
  | _ -> failwith "Invalid opcode"

let print_state a b c i opc opr debug =
  Stdio.printf "State: a=%d, b=%d, c=%d, i=%d, opc=%d, opr=%d, debug=%s\n" a b c
    i opc opr debug

let step a b c opc literal_opr i =
  let combo_opr = combo_operand literal_opr a b c in
  match opc with
  | 0 ->
      (* adv *)
      let numerator = a in
      let denominator = Base.Int.pow 2 combo_opr in
      let a = Base.Int.(numerator / denominator) in
      (a, b, c, i + 2, None)
  | 1 ->
      (* bxl *)
      let b = Int.bit_xor b literal_opr in
      (a, b, c, i + 2, None)
  | 2 ->
      (* bst *)
      let b = combo_opr % 8 in
      (a, b, c, i + 2, None)
  | 3 ->
      (* jnz *)
      if a <> 0 then (a, b, c, literal_opr, None) else (a, b, c, i + 2, None)
  | 4 ->
      (* bxc *)
      let b = Int.bit_xor b c in
      (a, b, c, i + 2, None)
  | 5 ->
      (* out *)
      let o = combo_opr % 8 in
      (a, b, c, i + 2, Some o)
  | 6 ->
      (* bdv *)
      let numerator = a in
      let denominator = Base.Int.pow 2 combo_opr in
      let b = Base.Int.(numerator / denominator) in
      (a, b, c, i + 2, None)
  | 7 ->
      (* cdv *)
      let numerator = a in
      let denominator = Base.Int.pow 2 combo_opr in
      let c = Base.Int.(numerator / denominator) in
      (a, b, c, i + 2, None)
  | _ -> failwith "Invalid operand"

let run a b c p =
  let rec run a b c p i acc =
    match List.nth p i with
    | Some opc -> (
        let opr = List.nth_exn p (i + 1) in
        let a, b, c, i, o = step a b c opc opr i in
        match o with
        | Some output -> run a b c p i (output :: acc)
        | None -> run a b c p i acc)
    | None -> List.rev acc
  in
  run a b c p 0 []

let solve_part1 input =
  let a, b, c, p = parse_input input in
  let o = run a b c p in
  String.concat ~sep:"," (List.map ~f:Int.to_string o)

let print_int_list lst =
  let s = String.concat ~sep:", " (List.map ~f:Int.to_string lst) in
  Stdio.printf "[%s]\n" s

let find_a b c p =
  let rec find_a a b c p n =
    let l = List.length p in
    let p' = List.drop p (l - n) in
    let rec try_i i =
      if i > 7 then -1
      else
        let a' = Int.shift_left a 3 + i in
        let o = run a' b c p in
        if List.equal Int.equal o p then a'
        else if List.equal Int.equal o p' then
          let res = find_a a' b c p (n + 1) in
          if res <> -1 then res else try_i (i + 1)
        else try_i (i + 1)
    in
    try_i 0
  in
  find_a 0 b c p 1

let solve_part2 input =
  let _, b, c, p = parse_input input in
  let a = find_a b c p in
  Int.to_string a
