open Core

let rec get_digit_string str line pos =
  if pos = String.length line then str else
  match String.get line pos with
  | c when Char.is_digit c -> get_digit_string (str ^ String.make 1 c) line (pos + 1)
  | _ -> str

let char_is_symbol c =
  match c with
  | '.' -> false
  | c when Char.is_digit c -> false
  | _ -> true

let is_bordering_symbol_on_line pos line =
  match line with
  | None -> false
  | Some line ->
    match pos with
    | pos when pos > 0 && char_is_symbol (String.get line (pos - 1)) -> true
    | pos when char_is_symbol (String.get line (pos)) -> true
    | pos when pos + 1 < String.length line && char_is_symbol (String.get line (pos + 1)) -> true
    | _ -> false

let is_bordering_symbol pos prev_line curr_line next_line =
  if
    is_bordering_symbol_on_line pos prev_line
    || is_bordering_symbol_on_line pos curr_line
    || is_bordering_symbol_on_line pos next_line then
  Some ()
  else None

let is_part_number digit_string pos prev_line curr_line next_line =
  Util.range_of_string digit_string
  |> List.find_map ~f:(
    fun i -> is_bordering_symbol (i + pos) prev_line curr_line next_line
  ) |> Util.bool_of_option

let process_digit prev_line curr_line next_line pos =
  let digit_string = get_digit_string "" curr_line pos in
  let next_pos = String.length digit_string + pos in
  let part_value = if is_part_number digit_string pos prev_line (Some (curr_line)) next_line then int_of_string digit_string else 0 in
  (next_pos, part_value)

let rec process_line prev_line curr_line next_line pos sum =
match pos with
| pos when pos >= String.length curr_line -> sum
| pos when Char.is_digit (String.get curr_line pos) ->
  let (next_pos, part_value) = process_digit prev_line curr_line next_line pos in
  process_line prev_line curr_line next_line next_pos (sum + part_value)
| pos -> process_line prev_line curr_line next_line (pos + 1) sum

let rec do_part_one sum prev_line curr_line next_line channel =
  match next_line with
  | None -> In_channel.close channel; process_line prev_line curr_line next_line 0 sum
  | Some next_line ->
      let sum = process_line prev_line curr_line (Some next_line) 0 sum in
      do_part_one sum (Some curr_line) next_line (In_channel.input_line channel) channel

let do_part_one sum channel =
  let curr_line = In_channel.input_line_exn channel in
  let next_line = In_channel.input_line channel in
  do_part_one sum None curr_line next_line channel

let part_one_answer = In_channel.create "./inputs/day_03.txt" |> do_part_one 0

