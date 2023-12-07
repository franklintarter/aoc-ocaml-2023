open Core

(* Time:      7  15   30 *)
(* Distance:  9  40  200 *)

let races = [(7, 9); (15, 40); (30, 200)]
(* let races = [(71530,940200)] *)
let get_winning_possibilities (t, d) =
  let possibilities = List.range 1 t in
  let distances = List.map possibilities ~f:(fun p -> p * (t - p)) in
  let winning_distances = List.filter distances ~f:(fun di -> di > d ) in
  List.length winning_distances

let part_one_answer =
  races |> List.map ~f:get_winning_possibilities |> List.fold ~init:1 ~f:(fun a w -> a * w)

let get_winning_possibilities2 (t, d) =
  let possibilities = List.range 1 t in
  let distances = List.map possibilities ~f:(fun p -> p * (t - p)) in
  let winning_distances = List.filter distances ~f:(fun di -> di > d ) in
  List.length winning_distances

let t = 59796575
let d = 597123410321328

(* Test *)
(* let t = 71530 *)
(* let d = 940200 *)

let is_winner b =
  let dis = (b * (t - b)) in
  dis > d

let rec find_right_inflection l r p =
    match is_winner p with
    | _ when r - l < 2 -> l
    | false -> find_right_inflection l p (l + ((p - l)/2))
    | true -> find_right_inflection p r (p + ((r - p)/2))

let rec find_inflection l r p =
  match is_winner p with
  | _ when r - l < 2 -> r
  | true -> find_inflection l p (l + ((p - l)/2))
  | false -> find_inflection p r (p + ((r - p)/2))

let part_two_answer =
  let half = t/2 in
  let left = find_inflection 0 half 0 in
  let right = find_right_inflection half t half in
  right - left + 1

