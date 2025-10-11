open Base

let parse_input lines = 
  List.map lines ~f:(String.split ~on:'-')

let parse_graph parsed =
  List.fold parsed ~init:(Map.Poly.empty) ~f:(
    fun acc edge ->
      match edge with
      | [a; b] ->
        let acc = Map.update acc a ~f:(function
          | None -> [b]
          | Some lst -> b :: lst
        ) in
        Map.update acc b ~f:(function
          | None -> [a]
          | Some lst -> a :: lst
        )
      | _ -> acc
  )
  |> Map.map ~f:(Hash_set.Poly.of_list)

let find_3_cliques graph =
  let cliques = ref [] in
  let nodes = Map.keys graph in
  List.iter nodes ~f:(fun a ->
    match Map.find graph a with
    | None -> ()
    | Some neighbors_a ->
      Hash_set.iter neighbors_a ~f:(fun b ->
        if String.(a < b) then
          match Map.find graph b with
          | None -> ()
          | Some neighbors_b ->
            Hash_set.iter neighbors_a ~f:(fun c ->
              if String.(b < c) && Hash_set.mem neighbors_b c && Hash_set.mem neighbors_a c then
                cliques := [a; b; c] :: !cliques
            )
      )
  );
  !cliques

let filter_t = List.filter ~f:(List.exists ~f:(String.is_prefix ~prefix:"t"))

let solve_part1 input =
  parse_input input
  |> parse_graph
  |> find_3_cliques
  |> filter_t
  |> List.length
  |> Int.to_string

let bron_kerbosch graph =
  let result = ref [] in
  let rec bk r p x =
    if Hash_set.is_empty p && Hash_set.is_empty x then
      result := (Hash_set.to_list r) :: !result
    else
      Hash_set.iter (Hash_set.copy p) ~f:(fun v ->
        let neighbors = Map.find graph v |> Option.value ~default:(Hash_set.Poly.create ()) in
        let r' = Hash_set.Poly.union r (Hash_set.Poly.of_list [v]) in
        let p' = Hash_set.Poly.inter p neighbors in
        let x' = Hash_set.Poly.inter x neighbors in
        bk r' p' x';
        Hash_set.remove p v;
        Hash_set.add x v;
      )
  in
  let r = Hash_set.Poly.create () in
  let p = Hash_set.Poly.of_list (Map.keys graph) in
  let x = Hash_set.Poly.create () in
  bk r p x;
  List.max_elt !result ~compare:(fun a b -> Int.compare (List.length a) (List.length b))

let solve_part2 input =
  parse_input input
  |> parse_graph
  |> bron_kerbosch
  |> Option.value_exn
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:","