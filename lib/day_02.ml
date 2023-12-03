open Core

type handful = { green: int; red: int; blue: int; };;
let empty_hand = { green = 0; red = 0; blue = 0 };;

let test_handful = "3 blue, 4 red"
let test_game = "8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"

let parse_color_in_handful h str =
  let s = String.strip str |> String.split ~on:' ' in
  match List.rev s with
| "green"::t -> { h with green = int_of_string (List.hd_exn t) }
| "red"::t -> { h with red = int_of_string (List.hd_exn t) }
| "blue"::t -> { h with blue = int_of_string (List.hd_exn t) }
| _ -> raise (Invalid_argument "")

let handful_of_string str = String.split str ~on:',' |> List.fold ~init:empty_hand ~f:parse_color_in_handful

let is_impossible_handful = function
  | { red; green; blue } when red > 12 || green > 13 || blue > 14 -> Some ()
  | _ -> None

let bool_of_option = function
  | Some _ -> true
  | None -> false

let is_impossible_game g = String.split g ~on:';' |> List.find_map ~f:(fun s -> handful_of_string s |> is_impossible_handful) |> bool_of_option

let parse_game_id str =
  match String.split str ~on:' ' with
  | _::t -> int_of_string (List.hd_exn t)
  | _ -> raise(Invalid_argument "")

let parse_game g = match String.split g ~on:':' with
| h::t -> (parse_game_id h, List.hd_exn t)
| _ -> raise (Invalid_argument "")

let process_line line =
  let (id, game) = parse_game(line) in
  if is_impossible_game game then 0
  else id

let rec do_part_one sum channel =
  match In_channel.input_line channel with
  | None -> In_channel.close channel; sum
  | Some line -> do_part_one ((process_line line) + sum) channel

let part_one_answer = In_channel.create "./inputs/day_02.txt" |> do_part_one 0

