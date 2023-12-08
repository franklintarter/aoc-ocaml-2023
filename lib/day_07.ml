open Core

let rec get_histogram map list = match list with
| h::t ->
  let v = Map.find map h |> Option.value_or_thunk ~default:(fun _ -> 0) in
  let map = Map.set map ~key:h ~data:(v + 1) in
  get_histogram map t
| [] -> Map.to_alist map

let get_type list =
  let f = List.map list ~f:snd |> List.sort ~compare:Int.compare in
  match f
with
| 5::_ -> 'Z'
| 4::_ -> 'Y'
| 3::2::_ -> 'X'
| 3::_ -> 'W'
| 2::2::_ -> 'V'
| 2::_ -> 'U'
| _ -> 'T'

let r = function
  | 'A' -> 'Z'
  | 'K' -> 'Y'
  | 'Q' -> 'X'
  | 'J' -> 'W'
  | 'T' -> 'V'
  | c -> c

let replace_with_orderable str = match str with
 | c1::c2::c3::c4::c5::_ -> String.of_list [r c1; r c2; r c3; r c4; r c5]
 | _ -> raise (Invalid_argument "")

let parse_hand line =
  let parts = String.split line ~on:' ' in
  let h = (String.to_list (List.hd_exn parts)) in
  let map = get_histogram Char.Map.empty h in
  let t = get_type map in
  let hand = String.make 1 t ^ replace_with_orderable h in
  (hand, int_of_string (List.nth_exn parts 1))

let rec parse_hands hands channel = match In_channel.input_line channel with
| None -> In_channel.close channel; hands
| Some line -> parse_hands ((parse_hand line)::hands) channel

let do_part_1 channel =
  let hands = parse_hands [] channel in
  let sorted = List.sort hands ~compare:(fun (h, _) (h2, _) -> String.compare h h2) in
  List.foldi sorted ~init:0 ~f:(fun i a (_,p) ->
    a + ((i+1) * p)
  )

let part_one_answer = In_channel.create "./inputs/day_07.txt" |> do_part_1

let rec get_histogram_and_joker_count map list js = match list with
| 'J'::t -> get_histogram_and_joker_count map t (js + 1)
| h::t ->
  let v = Map.find map h |> Option.value_or_thunk ~default:(fun _ -> 0) in
  let map = Map.set map ~key:h ~data:(v + 1) in
  get_histogram_and_joker_count map t js
| [] -> (Map.to_alist map, js)

let get_type list js =
  let sorted = List.map list ~f:snd |> List.sort ~compare:Int.compare in
  match match sorted with
    | h::t -> h+js::t
    | [] -> [5]
  with
  | 5::_ -> 6
  | 4::_ -> 5
  | 3::2::_ -> 4
  | 3::_ -> 3
  | 2::2::_ -> 2
  | 2::_ -> 1
  | _ -> 0

let r = function
  | 'A' -> 'Z'
  | 'K' -> 'Y'
  | 'Q' -> 'X'
  | 'J' -> '0'
  | 'T' -> 'V'
  | c -> c

let replace_with_orderable str = match str with
 | c1::c2::c3::c4::c5::_ -> String.of_list [r c1; r c2; r c3; r c4; r c5]
 | _ -> raise (Invalid_argument "")

let parse_hand line =
  let parts = String.split line ~on:' ' in
  let h = (String.to_list (List.hd_exn parts)) in
  let (hist, jokers) = get_histogram_and_joker_count Char.Map.empty h 0 in
  let t = get_type hist jokers in
  let hand = string_of_int t ^ replace_with_orderable h in
  print_endline hand;
  (hand, int_of_string (List.nth_exn parts 1))

let rec parse_hands hands channel = match In_channel.input_line channel with
| None -> In_channel.close channel; hands
| Some line -> parse_hands ((parse_hand line)::hands) channel

let do_part_2 channel =
  let hands = parse_hands [] channel in
  let sorted = List.sort hands ~compare:(fun (h, _) (h2, _) -> String.compare h h2) in
  List.foldi sorted ~init:0 ~f:(fun i a (h,p) ->
    print_endline h;
    print_endline (string_of_int p);
    print_endline (string_of_int i);
  
    a + ((i+1) * p)
  )

let part_two_answer = In_channel.create "./inputs/day_07.txt" |> do_part_2


