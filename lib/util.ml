open Core

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

let bool_of_option = function
  | Some _ -> true
  | None -> false

let range_of_string s = List.range 0 (String.length s);;

