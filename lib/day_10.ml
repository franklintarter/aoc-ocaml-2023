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
    if opens_toward (opposing_border border) (row , col) grid
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

(* Put pipe in a map *)
(* Walk lines with fun algo *)

let rec make_pipe_map (startr, startc) p1 grid map =
    let (b1, r1, c1) = p1 in
    match (r1 = startr && c1 = startc) with
    | true -> map
    | false ->
    let map = Map.add_exn map ~key:((string_of_int r1) ^ ":" ^ (string_of_int c1)) ~data:(char_at grid (r1, c1)) in
    let p1conn = borders_except (r1,c1) (opposing_border b1) grid |> find_connection (r1,c1) grid in
    make_pipe_map (startr, startc) p1conn grid map

let is_pipe map (r,c) =(Map.find map ((string_of_int r) ^ ":" ^ (string_of_int c)))|> Option.is_some
let get_pipe_char map (r,c) =(Map.find map ((string_of_int r) ^ ":" ^ (string_of_int c))) |> Option.value_exn

let get_ground_squares lines map =
    List.foldi lines ~init:[] ~f:(fun i acc line
        -> List.append (List.foldi (String.to_list line) ~init:[] ~f:(fun j acc _
            -> if not (is_pipe map (i,j)) then (i,j)::acc else acc )) acc)

let rec check_ground_square pos map inside on_pipe = match pos with
    | (r, c) when r < 0 || c < 0 -> (not on_pipe) && inside
    | (r, c) ->
        let next = check_ground_square (r - 1,c - 1) map in
        let on_pipe = is_pipe map (r,c)  in
        let pipe_char = if on_pipe then get_pipe_char map (r,c) else '.' in
        match on_pipe with
        | true when (char_not_eq 'L' pipe_char) && (char_not_eq '7' pipe_char) -> next (not inside) on_pipe
        | _ -> next inside on_pipe

let check_ground_squares gs map = List.fold gs ~f:(fun acc g -> if check_ground_square g map false false then acc + 1 else acc) ~init:0

let do_part_two channel =
    let lines = In_channel.input_lines channel in
    let (row, col) = List.find_mapi_exn lines ~f:(fun i l -> List.find_mapi (String.to_list l) ~f:(fun j c -> match c with | 'S' -> Some (i,j) | _ -> None) ) in
    let grid = List.map lines ~f:String.to_array |> List.to_array in
    let bs = get_borders (row, col) grid in
    List.iter bs ~f:(fun (b, r,c) -> printf "%c %d,%d, %c \n" b r c (char_at grid (r,c)));
    let conn = find_connection (row, col) grid bs in
    let map = make_pipe_map (row, col) conn grid String.Map.empty in
    let gs = get_ground_squares lines map in
    (* printf "ground squares %d \n" (List.length gs); *)
    check_ground_squares gs map

let part_two_answer = In_channel.create "./inputs/day_10.txt" |> do_part_two

(* Interesting but had to look this up and my diagonal ray cast worked once i figured out the perpendicular diagonal crosses count as two (or 0) crosses *)
let pick interior_vertices boundary_vertices = interior_vertices + (boundary_vertices / 2) - 1
let pick_prime area boundary_vertices = area - (boundary_vertices / 2) - 1

