open Core

let get_seeds line = String.split line ~on:':' |> List.last_exn |> String.strip |> String.split ~on:' ' |> List.map ~f:(fun s -> int_of_string s)

let rec add_map_entry channel m = match In_channel.input_line channel with
  | None -> In_channel.close channel; m
  | Some "" -> m
  | Some line ->
    let parts = String.split ~on:' ' line in
    match parts with
      | dest::source::length::_ -> add_map_entry channel ((int_of_string dest, int_of_string source, int_of_string length)::m)
      | _ -> raise (Invalid_argument "")

let skip_line channel =
  let _ = In_channel.input_line_exn channel in
  channel

let rec resolve_map map id =
  match map with
  | [] ->
    id
  | (dest, source, length)::_ when id >= source && id < source + length
    ->
    id - source + dest
  | (_, _, _)::t ->
      resolve_map t id

let do_part_one channel =
  let seeds = get_seeds (In_channel.input_line_exn channel) in
  let channel = channel |> skip_line |> skip_line in
  let seed_soil_map =  add_map_entry channel [] in
  let channel = channel |> skip_line in
  let soil_fert_map =  add_map_entry channel [] in
  let channel = channel |> skip_line in
  let fert_water_map =  add_map_entry channel [] in
  let channel = channel |> skip_line in
  let water_light_map =  add_map_entry channel [] in
  let channel = channel |> skip_line in
  let light_temp_map =  add_map_entry channel [] in
  let channel = channel |> skip_line in
  let temp_hum_map =  add_map_entry channel [] in
  let channel = channel |> skip_line in
  let hum_loc_map =  add_map_entry channel [] in
  let locs = List.map seeds ~f:(fun s ->
    resolve_map seed_soil_map s |> resolve_map soil_fert_map |> resolve_map fert_water_map |> resolve_map water_light_map |> resolve_map light_temp_map |> resolve_map temp_hum_map |> resolve_map hum_loc_map
  ) in
  List.min_elt  locs ~compare:(fun a b -> if a < b then -1 else 1) |> Option.value_exn


let part_one_answer = In_channel.create "./inputs/day_05.txt" |> do_part_one

(* Part 2 *)

let rec parse_seed_pairs seeds seed_pairs = match seeds with
  | h::h2::t -> parse_seed_pairs t ((h, h2)::seed_pairs)
  | [] -> seed_pairs
  | _ -> raise (Invalid_argument "")

let seed_range l = List.map l ~f:(fun (id, length) ->  List.range id (id + length + 1))
  |> List.fold ~init:[] ~f:(fun a l -> List.append a l)

let sort_map m = List.sort m ~compare:(fun (d, _, _) (d2, _, _) -> if d < d2 then -1 else 1)

let rec resolve_map_rev map id =
  match map with
  | [] ->
    id
  | (source, dest, length)::_ when id >= source && id < source + length
    ->
    id - source + dest
  | (_, _, _)::t ->
      resolve_map_rev t id

let do_part_two channel =
  let sp = parse_seed_pairs (get_seeds (In_channel.input_line_exn channel)) [] in
  let channel = channel |> skip_line |> skip_line in
  let seed_soil_map =  add_map_entry channel [] |> sort_map in
  let channel = channel |> skip_line in
  let soil_fert_map =  add_map_entry channel [] |> sort_map in
  let channel = channel |> skip_line in
  let fert_water_map =  add_map_entry channel [] |> sort_map in
  let channel = channel |> skip_line in
  let water_light_map =  add_map_entry channel [] |> sort_map in
  let channel = channel |> skip_line in
  let light_temp_map =  add_map_entry channel [] |> sort_map in
  let channel = channel |> skip_line in
  let temp_hum_map =  add_map_entry channel [] |> sort_map in
  let channel = channel |> skip_line in
  let hum_loc_map =  add_map_entry channel [] |> sort_map in

  (* The range of the 0 dest location map *)
  let seeds = List.range 0 22546860 in

  List.find_mapi seeds ~f:(fun s i ->
    let seed = s
    |> resolve_map_rev hum_loc_map
    |> resolve_map_rev temp_hum_map
    |> resolve_map_rev light_temp_map
    |> resolve_map_rev water_light_map
    |> resolve_map_rev fert_water_map
    |> resolve_map_rev soil_fert_map
    |> resolve_map_rev seed_soil_map
    in
    List.find_map sp ~f:(fun (b, e) -> if seed >= b && seed < b + e then Some i else None)
  ) |> Option.value_exn

(* Not quite brutte force cause i don't think that's possible, come back someday lolno? *)
(* If it was worst case, that would be a really unnatural distribution, idk. Got it solved. *)
(* 36 second run time :) *)
(* let part_two_answer = In_channel.create "./inputs/day_05.txt" |> do_part_two *)



