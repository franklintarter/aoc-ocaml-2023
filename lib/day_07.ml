open Core

let rec get_map map list = match list with
| h::t ->
  let v = Map.find map h |> Option.value_or_thunk ~default:(fun _ -> 0) in
  let map = Map.set map ~key:h ~data:(v + 1) in
  get_map map t
| [] -> Map.to_alist map

let get_type list =
  let f = List.map list ~f:(fun (_, n) -> n ) |> List.sort ~compare:(fun a b -> if a > b then -1 else 1) in
  Util.print_int_list f;
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
  let map = get_map Char.Map.empty h in
  let t = get_type map in
  let hand = String.make 1 t ^ replace_with_orderable h in
  (hand, int_of_string (List.nth_exn parts 1))

let rec parse_hands hands channel = match In_channel.input_line channel with
| None -> In_channel.close channel; hands
| Some line -> parse_hands ((parse_hand line)::hands) channel

let do_part_1 channel =
  let hands = parse_hands [] channel in
  let sorted = List.sort hands ~compare:(fun (h, _) (h2, _) -> String.compare h h2) in
  List.foldi sorted ~init:0 ~f:(fun i a (h,p) ->
    print_endline h;
    print_endline (string_of_int p);
    print_endline (string_of_int i);
  
    a + ((i+1) * p)
  )

let part_one_answer = In_channel.create "./inputs/day_07.txt" |> do_part_1

