
open Advent_of_code;;
let test = false
let data = get_input 2025 5

let tdata = String.split_on_char '\n'
"3-5
10-14
16-20
12-18

1
5
8
11
17
32"


let rec split_list l = match l with 
    | "" :: q -> [], q
    | x :: q -> let l1, l2 = split_list q in x::l1, l2
    | _ -> [], []

let rec check_ranges (l: (int* int) list) x =
    match l with
    | [] -> false 
    | (a, b) :: q -> (a <= x && x <= b) || check_ranges q x



(* PART 2: *)

let intersect (a1, b1) (a2, b2) = a1 <= b2 && a2 <= b1

let interval_merge (a1, b1) (a2, b2) =
    if intersect (a1, b1) (a2, b2) then Some (min a1 a2, max b1 b2) else None


(* Turn l into a list of disjoint intervals *)
let rec total_union (l: (int* int) list) : (int*int) list =
    match l with 
    | [] | [_] -> l
    | x :: q -> 
        let q' = total_union q in
        let to_merge = List.filter (intersect x) q' in
        let big_x = List.fold_left (fun r y -> Option.get (interval_merge y r)) x to_merge in
        big_x :: List.filter (Fun.negate (intersect big_x)) q'


let day_5 (data: string list) =
    let ranges, ingredients = split_list data in
    let ranges = 
        ranges |> List.map (String.split_on_char '-') 
        |> List.map (fun (x::y::_) -> (int_of_string x, int_of_string y))
    in
(*     let ingredients = ingredients |> List.map (int_of_string) in
    List.map (check_ranges ranges) ingredients |> List.filter (Fun.id) |> List.length *)
    total_union ranges |> List.map (fun (x, y) -> y-x+1) |> List.fold_left (+) 0

let main () =
    let sol = if test then -1 else day_5 tdata in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()