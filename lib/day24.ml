open Base

module Wires = struct
  type t = bool Map.M(String).t [@@deriving sexp]

  let parse (line: string): (string * bool) =
    let parts = String.split ~on:' ' (String.strip line) in
    match parts with
    | [name; value] -> (String.sub name ~pos:0 ~len:(String.length name - 1), String.equal value "1")
    | _ -> failwith "Invalid wire format"

  let output (wires: t) : int =
    Map.filter_keys wires ~f:(String.is_prefix ~prefix:"z")
    |> Map.to_alist
    |> List.sort ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2)
    |> List.map ~f:snd
    |> List.foldi ~init:0 ~f:(fun i acc b -> (acc + 2 ** i * (if b then 1 else 0)))
end

module Gate = struct
  type t = (* input1 input2 output *)
    | AND of string * string * string
    | OR of string * string * string
    | XOR of string * string * string
    [@@deriving sexp]

  let get_output = function
    | AND (_, _, output) | OR (_, _, output) | XOR (_, _, output) -> output

  let apply (cables: Wires.t) (gate: t) : Wires.t =
    let open Bool in
    match gate with
    | AND (input1, input2, output) -> 
      let i1 = Map.find cables input1
      and i2 = Map.find cables input2 in
      (match (i1, i2) with
      | (Some v1, Some v2) -> Map.set cables ~key:output ~data:(v1 && v2)
      | _ -> cables)
    | OR (input1, input2, output) ->
      let i1 = Map.find cables input1
      and i2 = Map.find cables input2 in
      (match (i1, i2) with
      | (Some v1, Some v2) -> Map.set cables ~key:output ~data:(v1 || v2)
      | _ -> cables)
    | XOR (input1, input2, output) ->
      let i1 = Map.find cables input1
      and i2 = Map.find cables input2 in
      (match (i1, i2) with
      | (Some v1, Some v2) -> Map.set cables ~key:output ~data:(v1 <> v2)
      | _ -> cables)

  let parse (line: string) : t =
    let parts = String.split ~on:' ' (String.strip line) in
    match parts with
    | [in1; gate; in2; "->"; out] -> (
        match gate with
        | "AND" -> AND (in1, in2, out)
        | "OR" -> OR (in1, in2, out)
        | "XOR" -> XOR (in1, in2, out)
        | _ -> failwith "Unknown gate type"
      )
    | _ -> failwith "Invalid gate format"
end

let parse_input (input: string list) : (Wires.t * Gate.t list) =
  let wires = List.take_while input ~f:(fun x -> not @@ String.equal "" x) in
  let gates = List.drop_while input ~f:(fun x -> not @@ String.equal "" x) |> List.tl_exn in
  (List.map wires ~f:Wires.parse |> Map.of_alist_exn (module String),
   List.map gates ~f:Gate.parse)

let simulate_pulse (wires: Wires.t) (gates: Gate.t list) : Wires.t =
    List.fold gates ~init:wires ~f:Gate.apply

let simulate_circuit (wires: Wires.t) (gates: Gate.t list) : Wires.t =
  let rec aux current_wires =
    let new_wires = simulate_pulse current_wires gates in
    if Map.equal Bool.equal current_wires new_wires then
      new_wires
    else
      aux new_wires
  in
  aux wires

let solve_part1 _input =
  let (wires, gates) = parse_input _input in
  let final_wires = simulate_circuit wires gates in
  Int.to_string (Wires.output final_wires)

let get_output_gates (gates: Gate.t list) (output: string) : Gate.t list =
  List.filter gates ~f:(fun gate ->
    match gate with
    | Gate.AND (input1, input2, _) | Gate.OR (input1, input2, _) | Gate.XOR (input1, input2, _)
      -> String.equal input1 output || String.equal input2 output
  )

let is_gate_valid (gate: Gate.t) (gates: Gate.t list): bool =
  let output_gates output =
    get_output_gates gates output
  in
  match gate with
  | Gate.AND (input1, input2, output) ->
      (match output_gates output with
      | [Gate.OR _] -> true
      | [Gate.AND _; Gate.XOR _] | [Gate.XOR _; Gate.AND _] ->
          (String.equal input1 "x00" && String.equal input2 "y00") ||
          (String.equal input1 "y00" && String.equal input2 "x00")
      | _ -> false)
  | Gate.OR (_, _, output) ->
      (match output_gates output with
      | [] -> String.equal output "z45"
      | [Gate.AND _; Gate.XOR _] | [Gate.XOR _; Gate.AND _] -> true
      | _ -> false)
  | Gate.XOR (input1, input2, output) ->
      (match output_gates output with
      | [] -> String.is_prefix ~prefix:"z" output
      | [Gate.AND _; Gate.XOR _] | [Gate.XOR _; Gate.AND _] ->
          (String.is_prefix ~prefix:"x" input1 && String.is_prefix ~prefix:"y" input2) ||
          (String.is_prefix ~prefix:"x" input2 && String.is_prefix ~prefix:"y" input1)
      | _ -> false)

let solve_part2 input =
  let (_, gates) = parse_input input in
  let valid_gates = List.filter gates ~f:(fun gate -> not (is_gate_valid gate gates)) in
  List.map valid_gates ~f:Gate.get_output
  |> List.sort ~compare:String.compare 
  |> String.concat ~sep:","