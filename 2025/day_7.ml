
open Advent_of_code;;
let test = false
let data = get_input 2025 7

let tdata = String.split_on_char '\n'
".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............."
let expected = 40

let rec insert (l: 'a list) (x: 'a)  =
    match l with 
    | [] -> [x]
    | y :: q -> if x < y then x :: l else if x = y then l else y :: insert q x

let rec splits_from 
    (rays: int list) (* columns of active rays, ordered *)
    (splitters: int list) (* list of columns of splitters, ordered *)
    (accu: int list) (* active rays for next iteration *)
    (counts: int) (* number of splits *)
    =
    match rays, splitters with
    | _, []  -> (List.fold_left insert accu rays, counts)
    | [], _ -> (accu, counts)
    | r :: rl, s :: sl ->
        (* print_int r; print_string " - "; print_int s; print_newline(); *)
        splits_from 
        (if r <= s then rl else rays) 
        (if r >= s then sl else splitters)  
        (if r = s then
            insert (insert accu (s+1)) (s-1) 
        else if r < s then
            insert accu r
        else
            accu)
        (if r = s then counts + 1 else counts)


(* PART 2: for each ray, remember # of timelines that could have created it*)

let rec insert (l: ('a * int) list) ((x, n): ('a * int))  =
    match l with 
    | [] -> [(x, n)]
    | (y, m) :: q -> if x < y then (x, n) :: l else if x = y then (x, n+m)::q else (y, m) :: insert q (x, n)

let rec splits_from 
    (rays: (int * int) list) (* columns of active rays, ordered. (ray, #timelines) *)
    (splitters: int list) (* list of columns of splitters, ordered *)
    (accu: (int* int) list) (* active rays for next iteration *)
    =
    match rays, splitters with
    | _, []  -> List.fold_left insert accu rays
    | [], _ -> accu
    | (r, n) :: rl, s :: sl ->
        splits_from 
        (if r <= s then rl else rays) 
        (if r >= s then sl else splitters)  
        (if r = s then
            insert (insert accu (s+1, n)) (s-1, n) 
        else if r < s then
            insert accu (r, n)
        else
            accu)


let day_7 (data: string list) =
    let rays = [(String.index (List.hd data) 'S', 1)] in
    let splitters = 
        List.tl data
        |> List.map (String.to_seq)
        |> List.map (List.of_seq)
        |> List.map (List.mapi (fun i c -> if c = '^' then Some i else None))
        |> List.map (List.filter_map Fun.id)
    in
    List.fold_left 
        (fun r sl -> 
            (* List.iter 
            (fun (x, n) -> print_int x; print_string " * "; print_int n; print_newline()) r;
            print_newline(); *)
            splits_from r sl []
        ) 
        rays 
        splitters 
    |> List.map snd  |> List.fold_left (+) 0


let main () =
    if day_7 tdata <> expected then
        print_endline "Wrong on test"
    else
    let sol = if test then -1 else day_7 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()