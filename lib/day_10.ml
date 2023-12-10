open Core

let get_borders (row, col) grid =
    let n = if row > 0 then Some ('N', row - 1, col) else None in
    let s = if col < (Array.length grid.(0)) - 1 then Some ('E', row, col + 1) else None in
    let e = if row < (Array.length grid) - 1 then Some ('S', row + 1, col) else None in
    let w = if col > 0 then Some ('W', row, col - 1 ) else None in
    [n;s;e;w] |> List.filter_opt

let char_at grid (row, col) = grid.(row).(col)

let eq_any c list = List.find list ~f:(fun c2 -> equal_char c c2) |> Option.is_some

let char_not_eq c c2 = int_of_char c <> int_of_char c2


let opens_toward dir (r,c) (grid: char array array) =
    printf "checking %d , %d \n" r c;
    let c = char_at grid (r,c) in
    (* printf "checking if %c opens %c \n" c dir; *)
    if equal_char c 'S' then true else
    match dir with
    | 'N' when eq_any c ['L'; 'J'; '|';] -> true
    | 'S' when eq_any c ['|'; 'F'; '7';] -> true
    | 'E' when eq_any c ['-'; 'L'; 'F';] -> true
    | 'W' when eq_any c ['-'; 'J'; '7';] -> true
    | _ -> false

let opposing_border = function
| 'N' -> 'S'
| 'S' -> 'N'
| 'E' -> 'W'
| 'W' -> 'E'
| _ -> raise (Invalid_argument "")

let connected p1 (border, row, col) grid =
    (* printf "Checking %c border \n" border; *)
    if opens_toward (opposing_border border)(row , col) grid
    && (opens_toward border p1 grid) then Some (border, row, col) else None

let find_connection p grid borders = List.find_map_exn borders ~f:(fun b -> connected p b grid)

let borders_except p dir grid =
    get_borders p grid |> List.filter ~f:(fun (b, _, _) -> char_not_eq b dir)

let rec walk p1 p2 grid steps =
    let (b1, r1, c1) = p1 in
    let (b2, r2, c2) = p2 in
    if (r1 = r2 && c1 = c2) then steps else
    let p1conn = borders_except (r1,c1) (opposing_border b1) grid |> find_connection (r1,c1) grid in
    let p2conn = borders_except (r2,c2) (opposing_border b2) grid |> find_connection (r2,c2) grid in
    walk p1conn p2conn grid (steps + 1)

let do_part_one channel =
    let lines = In_channel.input_lines channel in
    let (row, col) = List.find_mapi_exn lines ~f:(fun i l -> List.find_mapi (String.to_list l) ~f:(fun j c -> match c with | 'S' -> Some (i,j) | _ -> None) ) in
    let grid = List.map lines ~f:String.to_array |> List.to_array in
    (* print_endline (String.make 1 (grid.(row).(col))); *)
    let bs = get_borders (row, col) grid in
    List.iter bs ~f:(fun (b, r,c) -> printf "%c %d,%d, %c \n" b r c (char_at grid (r,c)));
    let conn = find_connection (row, col) grid bs in
    let other_conn = List.filter bs ~f:(fun (b, _, _) -> char_not_eq b (Tuple3.get1 conn)) |> find_connection (row, col) grid in
    walk conn other_conn grid 1

(* let part_one_answer = In_channel.create "./inputs/scratch.txt" |> do_part_one *)
let part_one_answer = In_channel.create "./inputs/day_10.txt" |> do_part_one

