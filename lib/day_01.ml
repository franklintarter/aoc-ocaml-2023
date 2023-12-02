open Core

(* Part 1 *)

let update_pair pair c = match pair with
  | (Some c1, Some _) -> (Some c1, Some c)
  | _ -> (Some c, Some c)

let reduce_string_to_numeric_char_pair pair c = match Char.is_digit c with
  | false -> pair
  | true -> update_pair pair c

let char_pair_or_fail p = match p with
  | (Some c1, Some c2) -> (c1, c2)
  | _ -> raise (Invalid_argument "Invalid Input")

let char_pair_of_line line = String.fold line ~f:reduce_string_to_numeric_char_pair ~init:(None, None)

let int_of_char_pair (c1, c2) = int_of_string (String.make 1 c1 ^ String.make 1 c2)

let int_from_line line = line |> char_pair_of_line |> char_pair_or_fail |> int_of_char_pair

let rec do_part_one sum channel =
  match In_channel.input_line channel with
  | None -> In_channel.close channel; sum
  | Some line ->
    do_part_one (sum + int_from_line line) channel

let part_one_answer = In_channel.create "./inputs/day_01.txt" |> do_part_one 0

(* Part 2 *)

let tokens = [
  "1", "1";
  "2", "2";
  "3", "3";
  "4", "4";
  "5", "5";
  "6", "6";
  "7", "7";
  "8", "8";
  "9", "9";
  "one", "1";
  "two", "2";
  "three", "3";
  "four", "4";
  "five", "5";
  "six", "6";
  "seven", "7";
  "eight", "8";
  "nine", "9";
];;

let match_token_at_pos str pos =
  List.find_map tokens ~f:(fun (substring, v) ->
    match String.is_substring_at str ~substring ~pos with
    | true -> Some v
    | false -> None
  );;

let match_token str range = List.find_map_exn range ~f:(fun pos -> match_token_at_pos str pos);;

let range_of_string s = List.range 0 (String.length s);;

let process_line str =
  let range = range_of_string str in
  let first =  match_token str range in
  let last = match_token str (List.rev range) in
  (* print_endline ((first ^ last) ^ " " ^ str); *)
  int_of_string (first ^ last)
;;

let rec do_part_two sum channel =
  match In_channel.input_line channel with
  | None -> In_channel.close channel; sum
  | Some line -> do_part_two (sum + (process_line line)) channel

let part_two_answer = In_channel.create "./inputs/day_01.txt" |> do_part_two 0

