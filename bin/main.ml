open Base

let read_input filename =
  In_channel.with_open_text filename In_channel.input_lines

let solutions = [|
  (Advent_of_code_2024.Day01.solve_part1, Advent_of_code_2024.Day01.solve_part2);
  (Advent_of_code_2024.Day02.solve_part1, Advent_of_code_2024.Day02.solve_part2);
  (Advent_of_code_2024.Day03.solve_part1, Advent_of_code_2024.Day03.solve_part2);
  (Advent_of_code_2024.Day04.solve_part1, Advent_of_code_2024.Day04.solve_part2);
  (Advent_of_code_2024.Day05.solve_part1, Advent_of_code_2024.Day05.solve_part2);
  (Advent_of_code_2024.Day06.solve_part1, Advent_of_code_2024.Day06.solve_part2);
  (Advent_of_code_2024.Day07.solve_part1, Advent_of_code_2024.Day07.solve_part2);
  (Advent_of_code_2024.Day08.solve_part1, Advent_of_code_2024.Day08.solve_part2);
  (Advent_of_code_2024.Day09.solve_part1, Advent_of_code_2024.Day09.solve_part2);
  (Advent_of_code_2024.Day10.solve_part1, Advent_of_code_2024.Day10.solve_part2);
  (Advent_of_code_2024.Day11.solve_part1, Advent_of_code_2024.Day11.solve_part2);
  (Advent_of_code_2024.Day12.solve_part1, Advent_of_code_2024.Day12.solve_part2);
  (Advent_of_code_2024.Day13.solve_part1, Advent_of_code_2024.Day13.solve_part2);
  (Advent_of_code_2024.Day14.solve_part1, Advent_of_code_2024.Day14.solve_part2);
  (Advent_of_code_2024.Day15.solve_part1, Advent_of_code_2024.Day15.solve_part2);
  (Advent_of_code_2024.Day16.solve_part1, Advent_of_code_2024.Day16.solve_part2);
  (Advent_of_code_2024.Day17.solve_part1, Advent_of_code_2024.Day17.solve_part2);
  (Advent_of_code_2024.Day18.solve_part1, Advent_of_code_2024.Day18.solve_part2);
  (Advent_of_code_2024.Day19.solve_part1, Advent_of_code_2024.Day19.solve_part2);
  (Advent_of_code_2024.Day20.solve_part1, Advent_of_code_2024.Day20.solve_part2);
  (Advent_of_code_2024.Day21.solve_part1, Advent_of_code_2024.Day21.solve_part2);
  (Advent_of_code_2024.Day22.solve_part1, Advent_of_code_2024.Day22.solve_part2);
  (Advent_of_code_2024.Day23.solve_part1, Advent_of_code_2024.Day23.solve_part2);
  (Advent_of_code_2024.Day24.solve_part1, Advent_of_code_2024.Day24.solve_part2);
  (Advent_of_code_2024.Day25.solve_part1, Advent_of_code_2024.Day25.solve_part2);
|]

let time f x =
  let start = Unix.gettimeofday () in
  let res = f x in
  let stop = Unix.gettimeofday () in
  let diff = stop -. start in
  (res, diff)

let solve day = 
  let filename = Printf.sprintf "input/day%02d.txt" day in
  let (input: string list) = read_input filename in
  Stdio.printf "Solutions for Day %d:\n%!" day;
  let (part1, part2) = solutions.(day - 1) in
  let result1, time1 = time part1 input in
  Stdio.printf "  Part 1: %s (%.4fs)\n%!" result1 time1;
  let result2, time2 = time part2 input in
  Stdio.printf "  Part 2: %s (%.4fs)\n%!" result2 time2

let () =
  match Stdlib.Sys.argv with
  | [|_|] ->
    let (_, t) = time (fun () ->
      for i = 1 to 25 do
        solve i;
        Stdio.printf "\n%!"
      done
      ) () in
    Stdio.printf "Total time: %.4fs\n" t
  | [|_; day|] ->
    Int.of_string day |> solve 
  | _ -> Stdio.printf "Usage: %s [day_number]\n" Stdlib.Sys.argv.(0)