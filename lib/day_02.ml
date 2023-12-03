open Core

type color_set = { green: int; red: int; blue: int; };;
let empty_color_set = { green = 0; red = 0; blue = 0 };;

let parse_color_string str =
  let strs = String.strip str |> String.split ~on:' ' in
  let (h, t) = (List.hd_exn strs, List.tl_exn strs) in
  (int_of_string h, List.hd_exn t)

let color_set_of_handful h str =
  match parse_color_string str with
  | (n, "green") -> { h with green = n }
  | (n, "red") -> { h with red = n }
  | (n, "blue") -> { h with blue = n }
  | _ -> raise (Invalid_argument "")

let color_set_of_handful_string str = String.split str ~on:',' |> List.fold ~init:empty_color_set ~f:color_set_of_handful

let is_impossible_color_set = function
  | { red; green; blue } when red > 12 || green > 13 || blue > 14 -> Some ()
  | _ -> None


let is_impossible_game game_string =
  String.split game_string ~on:';'
  |> List.find_map ~f:(fun s -> color_set_of_handful_string s |> is_impossible_color_set)
  |> Util.bool_of_option

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

(* Part 2 *)

let set_min color_set (number, color) = match color with
| "green" when number > color_set.green -> { color_set with green = number }
| "red" when number > color_set.red -> { color_set with red = number }
| "blue" when number > color_set.blue -> { color_set with blue = number }
| _ -> color_set

let parse_color_string str =
  let strs = String.strip str |> String.split ~on:' ' in
  let (h, t) = (List.hd_exn strs, List.tl_exn strs) in
  (int_of_string h, List.hd_exn t)

let apply_handful color_set str =
  String.split str ~on:','
  |> List.map ~f:String.strip
  |> List.map ~f:parse_color_string
  |> List.fold ~init:color_set ~f:set_min

let find_min_color_set = List.fold ~init:empty_color_set ~f:apply_handful

let mult_color_set s = s.green * s.red * s.blue

let process_line2 line =
  let (_, game_string) = parse_game line in
  let handful_strings = String.split ~on:';' game_string in
  let min_color_set = find_min_color_set handful_strings in
  mult_color_set min_color_set

let rec do_part_two sum channel =
  match In_channel.input_line channel with
  | None -> In_channel.close channel; sum
  | Some line -> do_part_two ((process_line2 line) + sum) channel

let part_two_answer = In_channel.create "./inputs/day_02.txt" |> do_part_two 0

(* don't really like how I'm mixing styles of functions, parsing with logic, but still experimenting with diff styles and language features *)

