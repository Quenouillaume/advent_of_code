
open Advent_of_code;;
let test = false
let data = get_input 2024 25


let rec regroup curr i l = 
    match i, l with
    | _, [] -> [List.rev curr]
    | 7, x::q -> List.rev curr :: regroup [] 0 q
    | _, x::q -> regroup (x::curr) (i+1) q

let is_key t = t.(0).[0] = '.'
let signature t =
        let s = Array.make 5 0 in
        for j = 0 to 4 do
            let i = ref 0 in
            while !i < 7 && t.(!i).[j] = t.(0).[j] do
                print_int j; print_string " "; print_int !i; print_newline();
                incr i
            done;
            if t.(0).[j] = '.' then s.(j) <- (6 - !i)
            else s.(j) <- (!i-1);
            print_int j; print_string "AA\n" 
        done;
        s

let keys_and_locks l =
    l |> regroup [] 0 |> List.map (Array.of_list) |>
    List.fold_left 
        (fun (locks, keys) t -> 
            let s = signature t in 
            if is_key t then (locks, s::keys) 
            else (s::locks, keys)
        ) ([],[])

let tdata = String.split_on_char '\n'
"#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####"

let tdata = String.split_on_char '\n'
".....
.....
.....
#....
#.#..
#.#.#
#####"

let print_sig s = 
    for i = 0 to 4 do
        print_int s.(i)
    done
    

let day_25 (data: string list) =
    let locks, keys = keys_and_locks  data in
    print_string "Done\n";
    let fits k l =
        let res = ref true in
        for i = 0 to 4 do
            if k.(i) + l.(i) > 5 then res := false 
        done;
        !res
    in
    let rec count_fits ks ls =
        match ks, ls with 
        | [], _ -> 0
        | k::qk, l::ql -> begin print_sig k; print_string " + "; print_sig l; 
            let a = (if fits k l then begin 
                print_string " fits\n";
                1
             end else begin 
                print_string " doesn't fit\n";
                 0
             end) in a + count_fits ks ql end
        | k::qk, [] -> count_fits qk locks
    in 
    print_string "Keys: \n";
    List.iter (fun s -> print_sig s; print_string", ") keys;
    print_string "\n\nLocks:\n";
    List.iter (fun s -> print_sig s; print_string", ") locks;
    print_newline();
    count_fits keys locks
        

let main () =
    let sol = if test then -1 else day_25 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()