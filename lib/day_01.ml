let zero = Char.code '0'
let nine = Char.code '9'

let is_char_numeric c = match Char.code c with
  | code when code >= zero && code <=nine -> true
  | _ -> false

let reduce_string_to_numeric_char_pair pairs c = match is_char_numeric c with
  | false -> pairs
  | true -> match pairs with
    | (Some c1, Some _) -> (Some c1, Some c)
    | _ -> (Some c, Some c)

let char_pair_or_fail p = match p with
  | (Some c1, Some c2) -> (c1, c2)
  | _ -> raise (Invalid_argument "Invalid Input")

let char_pair_of_line line =
  String.fold_left (reduce_string_to_numeric_char_pair) (None, None) line

let int_of_char_pair (c1, c2) = int_of_string (String.make 1 c1 ^ String.make 1 c2)

let int_from_line line = line |> char_pair_of_line |> char_pair_or_fail |> int_of_char_pair

let rec process sum channel =
  match Util.next_line channel with
  | None -> close_in channel; sum
  | Some line ->
    process (sum + int_from_line line) channel

let part_one_answer = open_in "./inputs/day_01.txt" |> process 0

