open Base

let print_warehouse grid =
  List.iter grid ~f:(fun row ->
      List.iter row ~f:(fun c -> Stdio.printf "%c" c);
      Stdio.printf "\n")

let print_moves chars =
  List.iter chars ~f:(fun c -> Stdio.printf "%c" c);
  Stdio.printf "\n"

let print_position (row, col) =
  Stdio.printf "Position: (%d, %d)\n" row col

let parse_input input =
  let warehouse, moves = List.split_while ~f:(fun s -> String.(s <> "")) input in
  let moves = List.tl_exn moves in

  let position = ref (-1, -1) in

  let parsed_warehouse =
    List.mapi warehouse ~f:(fun y line ->
      String.to_list line
      |> List.mapi ~f:(fun x char ->
          if Char.(char = '@') then (
            position := (x, y);
            '.'
          ) else char))
  in

  let parsed_moves =
    List.concat_map moves ~f:String.to_list
  in

  (parsed_warehouse, parsed_moves, !position)

let char_at warehouse position =
  let x, y = position in
  List.nth_exn (List.nth_exn warehouse y) x

let move warehouse position direction =
  let calc_positions warehouse (x, y) direction =
    let calc_positions_helper warehouse x y dx dy =
        let x1, y1 = x + dx, y + dy in
        let next = char_at warehouse (x1, y1) in

        let rec calc_pos2 warehouse x y dx dy =
          let nx, ny = x + dx, y + dy in
          let next = char_at warehouse (nx, ny) in
          match next with
          | '.' -> Some (nx, ny)
          | 'O' -> calc_pos2 warehouse nx ny dx dy
          | '#' | _ -> None
        in

        match next with
        | '.' -> Some ((x1, y1), (x, y))
        | 'O' -> (
          match calc_pos2 warehouse x1 y1 dx dy with
          | None -> None
          | Some (x2, y2) -> Some ((x1, y1), (x2, y2)))
        | '#' | _ -> None
    in

    match direction with
    | '^' -> calc_positions_helper warehouse x y 0 (-1)
    | '>' -> calc_positions_helper warehouse x y 1 0
    | 'v' -> calc_positions_helper warehouse x y 0 1
    | '<' -> calc_positions_helper warehouse x y (-1) 0
    | _ -> None
  in

  let swap_positions warehouse (x1, y1) (x2, y2) =
    List.mapi warehouse ~f:(fun y row ->
        List.mapi row ~f:(fun x char ->
            if x = x1 && y = y1 then List.nth_exn (List.nth_exn warehouse y2) x2
            else if x = x2 && y = y2 then List.nth_exn (List.nth_exn warehouse y1) x1
            else char))
  in

  match calc_positions warehouse position direction with
  | None -> warehouse, position
  | Some (pos1, pos2) -> (swap_positions warehouse pos1 pos2, pos1)

let solve_part1 (input: string list) = 
  let warehouse, moves, position = parse_input input in
  let warehouse, _ =
    List.fold moves ~init:(warehouse, position) ~f:(fun (current_warehouse, current_position) direction ->
      move current_warehouse current_position direction)
  in
  List.mapi warehouse ~f:(fun y row ->
    List.mapi row ~f:(fun x char -> 
      if Char.(char = 'O') then x + 100 * y else 0))
  |> List.concat
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string


let fix_warehouse_part2 warehouse =
  List.map warehouse ~f:(fun row ->
    List.concat_map row ~f:(function
        | '#' -> ['#'; '#']
        | '.' -> ['.'; '.']
        | 'O' -> ['['; ']']
        | c -> [c]))

let move_part2 warehouse (x, y) direction =
  (* Stdio.printf "Direction: %c\n" direction;
  print_position (x, y);
  print_warehouse warehouse; *)

  let get_box warehouse (x, y) =
    match char_at warehouse (x, y) with
    | '[' -> (x, y)
    | ']' -> (x - 1, y)
    | _ -> (-1, -1)
  in
  
  let move_box warehouse (x, y) (dx, dy) =
    let nx, ny = get_box warehouse (x + dx, y + dy) in

    let rec find_boxes warehouse (nx, ny) (dx, dy) =
      match (dx, dy) with
      | 0, _ -> (
        let nextl = char_at warehouse (nx, ny + dy) in
        let nextr = char_at warehouse (nx + 1, ny + dy) in
        let left = 
          match nextl with
          | '.' -> []
          | '#' -> [(-1, -1, -1, -1)]
          | '[' | ']' ->
            let (blx, bly) = get_box warehouse (nx, ny + dy) in
            (blx, bly, blx, bly + dy) :: find_boxes warehouse (blx, bly) (dx, dy)
          | _ -> failwith "wrong input"
          in
        let right = 
          match nextr with
          | '.' -> []
          | '#' -> [(-1, -1, -1, -1)]
          | '[' | ']' ->
            let (blx, bly) = get_box warehouse (nx + 1, ny + dy) in
            (blx, bly, blx, bly + dy) :: find_boxes warehouse (blx, bly) (dx, dy)
          | _ -> failwith "wrong input"
        in
        left @ right)
      | -1, 0 -> (
        let next = char_at warehouse (nx + dx, ny) in
        match next with
        | '.' -> []
        | '#' -> [(-1, -1, -1, -1)]
        | '[' | ']' ->
          let (bx, by) = get_box warehouse (nx + dx, ny) in
          (bx, by, bx + dx, by) :: find_boxes warehouse (bx, by) (dx, dy)
        | _ -> failwith "wrong input"
        )
      | 1, 0 -> (
          let next = char_at warehouse (nx + dx + 1, ny) in
          match next with
          | '.' -> []
          | '#' -> [(-1, -1, -1, -1)]
          | '[' | ']' ->
            let (bx, by) = get_box warehouse (nx + dx + 1, ny) in
            (bx, by, bx + dx, by) :: find_boxes warehouse (bx, by) (dx, dy)
          | _ -> failwith "wrong input"
          )
      | _ -> []
    in

    let boxes = (nx, ny, nx + dx, ny + dy) :: find_boxes warehouse (nx, ny) (dx, dy) in

    let equal_swap (a1, b1, c1, d1) (a2, b2, c2, d2) = 
      a1 = a2 && b1 = b2 && c1 = c2 && d1 = d2
    in

    if List.mem boxes (-1,-1,-1,-1) ~equal:equal_swap then(
      warehouse, (x, y))
    else
      let warehouse = List.mapi warehouse ~f:(fun y row ->
        List.mapi row ~f:(fun x char ->
            if List.mem boxes (x - 1, y, x - 2, y) ~equal:equal_swap 
              || List.mem boxes (x + 1, y, x + 2, y) ~equal:equal_swap
              || List.mem boxes (x, y, x, y - 1) ~equal:equal_swap
              || List.mem boxes (x - 1, y, x - 1, y - 1) ~equal:equal_swap
              || List.mem boxes (x, y, x, y + 1) ~equal:equal_swap
              || List.mem boxes (x - 1, y, x - 1, y + 1) ~equal:equal_swap then '.'
            else char)) in
      let warehouse = List.mapi warehouse ~f:(fun y row ->
        List.mapi row ~f:(fun x char ->
            if List.mem boxes (0, 0, x, y) ~equal:(fun (_, _, c1, d1) (_, _, c2, d2) -> c1 = c2 && d1 = d2) then '['
            else if List.mem boxes (0, 0, x - 1, y) ~equal:(fun (_, _, c1, d1) (_, _, c2, d2) -> c1 = c2 && d1 = d2) then ']'
            else char)) in
      warehouse, (x + dx, y + dy)
  in

  let dx, dy =
    match direction with
    | '^' ->  0, (-1)
    | '>' -> 1, 0
    | 'v' -> 0, 1
    | '<' -> (-1), 0
    | _ -> 0, 0
  in

  let nx, ny = x + dx, y + dy in
  let next = char_at warehouse (nx, ny) in

  match next with
  | '.' -> warehouse, (nx, ny)
  | '[' | ']' -> move_box warehouse (x, y) (dx, dy)
  | '#' | _ -> warehouse, (x, y)

let solve_part2 (input: string list) =
  let warehouse, moves, position = parse_input input in
  let warehouse = fix_warehouse_part2 warehouse in
  let position = (fun (x, y) -> x * 2, y) position in
  let warehouse, _ =
    List.fold moves ~init:(warehouse, position) ~f:(fun (current_warehouse, current_position) direction ->
      move_part2 current_warehouse current_position direction)
  in
  List.mapi warehouse ~f:(fun y row ->
    List.mapi row ~f:(fun x char -> 
      if Char.(char = '[') then x + 100 * y else 0))
  |> List.concat
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string