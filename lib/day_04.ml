open Core

(* Part one *)

let parse_string_nums str =
  String.strip str |> String.split ~on:' ' |> List.map ~f:String.strip |> List.filter ~f:(fun s -> not (String.is_empty s)) |> List.map ~f:int_of_string

let process_line line =
  let nums = String.split ~on:':' line |> List.last_exn |> String.split ~on:'|' in
  let winning_nums = List.hd_exn nums |> parse_string_nums |> List.fold ~f:(fun a n -> Set.add a n) ~init:Int.Set.empty in
  let my_nums = List.last_exn nums |> parse_string_nums in
  let nums_to_count = List.filter my_nums ~f:(fun n -> Set.mem winning_nums n) in
  List.fold nums_to_count ~init:0 ~f:(fun a _ -> if a = 0 then 1 else a * 2)

let rec do_part_one sum channel =
  match In_channel.input_line channel with
  | None -> In_channel.close channel; sum
  | Some line -> do_part_one ((process_line line) + sum) channel

let part_one_answer = In_channel.create "./inputs/day_04.txt" |> do_part_one 0

(* Part 2 *)

let process_line_2 line =
  let nums = String.split ~on:':' line |> List.last_exn |> String.split ~on:'|' in
  let winning_nums = List.hd_exn nums |> parse_string_nums |> List.fold ~f:(fun a n -> Set.add a n) ~init:Int.Set.empty in
  let my_nums = List.last_exn nums |> parse_string_nums in
  let nums_to_count = List.filter my_nums ~f:(fun n -> Set.mem winning_nums n) in
  List.length nums_to_count
  
let rec build_lookup_table pos map channel =
  match In_channel.input_line channel with
  | None -> In_channel.close channel; map
  | Some line ->
      let map = Map.add_exn map ~key:pos ~data:(process_line_2 line) in
      build_lookup_table (pos + 1) map channel

let rec process_queue map tally q = match q with
  | q when Queue.is_empty q -> tally
  | q ->
    let card = Queue.dequeue_exn q in
    let new_card_count = Map.find_exn map card in
    Queue.enqueue_all q (List.range ~stop:`inclusive (card + 1) (card + new_card_count));
    process_queue map (tally + 1) q

let part_two_answer =
  let map = In_channel.create "./inputs/day_04.txt" |> build_lookup_table 0 Int.Map.empty in
  let q = Queue.init (Map.length map) ~f:( fun i -> i ) in
  process_queue map 0 q


