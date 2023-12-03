open Core

(* let next_line channel = *)
(*   try *)
(*     let line = In_channel.input_line channel in *)
(*     Some line *)
(*   with | End_of_file -> *)
(*     None *)

let join strings = List.fold strings ~init:"" ~f:(fun a i -> a ^ "," ^ i);;
let str_list_of_int_list l = List.map l ~f:string_of_int
let print_int_list l = l |> str_list_of_int_list |> join |> print_endline;;
let print_string_list l = l |> join |> print_endline;;

let print_option_int = function
  | None -> print_endline "None"
  | Some n -> print_endline (string_of_int n)
;;

let print_option_string = function
  | None -> print_endline "None"
  | Some n -> print_endline n
;;

