open Core

let is_loc_char c = Char.is_alpha c || Char.is_digit c

let rec parse_map_entries map channel = match In_channel.input_line channel with
| None -> map
| Some line ->
  let key = String.split line ~on:'=' |> List.hd_exn |> String.strip in
  let data = match String.split line ~on:'=' |> List.last_exn |> String.strip |> String.split ~on:',' with
    | l::r::[] -> String.filter l ~f:is_loc_char, String.filter r ~f:is_loc_char
    | _ -> raise (Invalid_argument "")
  in
  (* printf "%s %s\n" (fst data) (snd data); *)
  parse_map_entries (Map.add_exn map ~key ~data) channel

let next_idx commands idx = if String.length commands - 1 = idx then 0 else idx + 1

let parse_input channel =
  let commands = In_channel.input_line_exn channel in
  let _ = In_channel.input_line_exn channel in
  (* printf "%s\n" commands; *)
  let map = parse_map_entries String.Map.empty channel in
  (* Map.to_alist map |> List.iter ~f:(fun (k, (l, r)) -> printf "%s - %s,%s\n" k l r ); *)
  commands, map

let rec walk count map position commands command_idx = match position with
| "ZZZ" -> count
| _ ->
    let (l, r) = Map.find_exn map position in
    let next_cmd = next_idx commands command_idx in
    let next_pos = match String.get commands command_idx with
      | 'L' -> l
      | 'R' -> r
      | _ -> raise(Invalid_argument "")
    in
    walk (count + 1) map next_pos commands next_cmd

let part_one_answer =
  let commands, map = In_channel.create "./inputs/day_08.txt" |> parse_input in
  walk 0 map "AAA" commands 0

let is_start loc = match String.to_list loc with
| _::_::'A'::[] -> true
| _ -> false

let is_end loc = match String.to_list loc with
| _::_::'Z'::[] -> true
| _ -> false

let rec walk count map position commands command_idx = match is_end position with
| true -> count
| _ ->
    let (l, r) = Map.find_exn map position in
    let next_cmd = next_idx commands command_idx in
    let next_pos = match String.get commands command_idx with
      | 'L' -> l
      | 'R' -> r
      | _ -> raise(Invalid_argument "")
    in
    walk (count + 1) map next_pos commands next_cmd

(* Brutte Force DNF obv *)
let rec ghost_walk count map positions commands command_idx =
  match List.for_all positions ~f:(fun p -> is_end p) with
  | true -> count
  | _ ->
      print_endline (string_of_int count);
      List.iter positions ~f:(fun p -> printf "%s " p);
      print_endline "";
      let next_cmd = next_idx commands command_idx in
      let command = String.get commands command_idx in
      let next_positions = positions |> List.map ~f:(fun p ->
        let (l, r) = Map.find_exn map p in
        match command with | 'L' -> l | _ -> r
      ) in
      ghost_walk (count + 1) map next_positions commands next_cmd

(* looked math stuff up *)
let rec gcd a b = if b = 0 then a else gcd b (a % b)
let lcm a b = if a = 0 || b = 0 then 0 else abs (a * b) / (gcd a b)
let rec lcm_of_list = function
  | [] -> 1
  | [h] -> h
  | a::b::t -> lcm_of_list ((lcm a b)::t)

let part_two_answer =
  let commands, map = In_channel.create "./inputs/day_08.txt" |> parse_input in
  let starts = Map.to_alist map |> List.filter ~f:(fun (k, _) -> is_start k) |> List.map ~f:(fun (k, _) -> k) in
  let finishes = starts |> List.map ~f:(fun s -> s, walk 0 map s commands 0) in
  List.iter finishes ~f:(fun (s, f) -> printf "%s - %d\n" s f ;);
  finishes |> List.map ~f:snd |> lcm_of_list

