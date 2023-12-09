open Core

let predict stack = Stack.fold stack ~init:0 ~f:(fun acc v -> acc + v)

let rec process_line stack next_layer nums = match nums with
  | [] -> raise(Invalid_argument "")
  | a::b::t -> process_line stack ((b-a)::next_layer) (b::t)
  | a::[] ->
    Stack.push stack a;
    match List.for_all next_layer ~f:(fun n -> n = 0) with
    | true -> predict stack
    | false ->
      process_line stack [] (List.rev next_layer)

let rec do_part_one sum channel = match In_channel.input_line channel with
| None -> In_channel.close channel; sum
| Some line ->
    let v = String.split line ~on:' ' |> List.map ~f:int_of_string |> process_line (Stack.create ()) [] in
    do_part_one (sum + v) channel

let part_one_answer = In_channel.create "./inputs/day_09.txt" |> do_part_one 0

(* Part 2 *)

let predict_rev stack = Stack.fold stack ~init:0 ~f:(fun acc v ->  v - acc)

let rec process_line2 stack next_layer nums = match nums with
  | [] -> raise(Invalid_argument "")
  | a::b::t -> process_line2 stack ((b-a)::next_layer) (b::t)
  | _::[] ->
    match List.for_all next_layer ~f:(fun n -> n = 0) with
    | true -> predict_rev stack
    | false ->
      let rev = List.rev next_layer in
      Stack.push stack (List.hd_exn rev);
      process_line2 stack [] rev

let rec do_part_two sum channel = match In_channel.input_line channel with
| None -> In_channel.close channel; sum
| Some line ->
    let stack = Stack.create () in
    let nums = String.split line ~on:' ' |> List.map ~f:int_of_string in
    Stack.push stack (List.hd_exn nums);
    let v = nums |> process_line2 stack [] in
    do_part_two (sum + v) channel

let part_two_answer = In_channel.create "./inputs/day_09.txt" |> do_part_two 0

