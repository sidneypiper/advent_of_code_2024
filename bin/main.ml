let read_input filename =
  let ic = open_in filename in  (* Open the file for reading *)
  let rec read_lines acc =
    try
      let line = input_line ic in  (* Read a line from the file *)
      read_lines (line :: acc)     (* Accumulate the lines in reverse order *)
    with End_of_file ->           (* Stop when the end of the file is reached *)
      close_in ic;                 (* Close the file after reading *)
      List.rev acc                  (* Return the lines in the original order *)
  in
  read_lines []                   (* Start reading the lines *)

let () =
  if Array.length Sys.argv < 3 then
    begin
      Printf.printf "Usage: %s <day> <input_file>\n" Sys.argv.(0);
      exit 1
    end;
  
  let day = int_of_string Sys.argv.(1) in
  let filename = Sys.argv.(2) in
  let (input: string list) = read_input filename in
    match day with
    | 1 -> 
        Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day01.solve_part1 input);
        Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day01.solve_part2 input);
    | 2 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day02.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day02.solve_part2 input);
    | 3 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day03.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day03.solve_part2 input);
    | 4 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day04.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day04.solve_part2 input);
    | 5 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day05.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day05.solve_part2 input);
    | 6 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day06.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day06.solve_part2 input);
    | 7 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day07.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day07.solve_part2 input);
    | 8 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day08.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day08.solve_part2 input);
    | 9 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day09.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day09.solve_part2 input);
    | 10 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day10.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day10.solve_part2 input);
    | 11 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day11.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day11.solve_part2 input);
    | 12 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day12.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day12.solve_part2 input);
    | 13 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day13.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day13.solve_part2 input);
    | 14 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day14.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day14.solve_part2 input);
    | 15 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day15.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day15.solve_part2 input);
    | 16 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day16.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day16.solve_part2 input);
    | 17 -> 
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day17.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day17.solve_part2 input);
    | 18 ->
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day18.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day18.solve_part2 input);
    | 19 ->
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day19.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day19.solve_part2 input);
    | 20 ->
      Printf.printf "Part 1: %s\n" (Advent_of_code_2024.Day20.solve_part1 input);
      Printf.printf "Part 2: %s\n" (Advent_of_code_2024.Day20.solve_part2 input);
    | _ -> Printf.printf "Day %d not implemented yet\n" day
