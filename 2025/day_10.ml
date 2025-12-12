open Advent_of_code;;
let test = false
let data = get_input 2025 10

let tdata = String.split_on_char '\n'
"[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
let expected = 7

(* bit flip i in positions b *)
let bit_flip (b: int list) (i: int) : int =
    b |> List.map ((lsl) 1) |> List.fold_left (lxor) i

(* read l as a base 2 int *)
let r2 (l: bool list): int = 
    l |> List.rev |> List.fold_left (fun r bi -> 2*r + (if bi then 1 else 0)) 0 ;;

let efficient_presses (objective: bool list) (buts: int list list) (volts: int list) =
    let n = 1 lsl (List.length objective) in
    let p = List.length buts in
    (* t j i k = best way to make i using only j first switchs  *)
    let t = Array.make_matrix (p+1) n (p+1) in
    for j = 0 to p do
        t.(j).(0) <-0
    done;  
    let rec fill_t (j: int) (buts: int list list) (volts: int list) =
        match buts, volts with
        | [], _| _,[] -> ()
        | b :: qb, v :: qv ->
            (* either press b or not *)
            Array.iteri 
                (fun i s ->
                    let i' = bit_flip b i in
                    t.(j).(i') <- min t.(j-1).(i') (v + t.(j-1).(i))
                )
                t.(j);
            fill_t (j+1) qb qv
    in
    fill_t 1 buts volts;
(*     for j = 0 to p do
        Printf.printf "Row %2d: " j;
        for i = 0 to n-1 do
            Printf.printf "[%d: %d] " i t.(j).(i)
        done;
        print_newline();     
    done; *)
    t.(p).(r2 objective)

(* Part 2 faster: As seen on reddit

First, compute all sets of button presses that match parity
of the objective voltage. then, for each set B,
recursively solve for half the remaning voltage.
Finally, check which choice gives a smaller solution

MDRRRR ça prend littéralement 0.5 secondes...

 *)
(* Return an array T s.t.
   T[i] is the list of sets of buttons that realise objective i *)
let all_presses (len: int) (buts: int list array): int list list array =
    let n = 1 lsl (len) in
    let p = Array.length buts in
    (* t i j = list of ways to make i using only buttons j to p-1  *)
    let t = Array.make_matrix n (p+1) None in
    t.(0).(p) <- Some [[]]; (* only one way to make 0 without buttons *)
    for i = 1 to n-1 do
        t.(i).(p) <- Some [] (* no way to make i without buttons *)
    done;  
    let rec fill_t (i: int) (j: int) : int list list  =
        if t.(i).(j) = None then begin
            let res = fill_t i (j+1) in
            let i' = bit_flip buts.(j) i in
            (* Printf.printf "%d %d %d | %d %d\n" i i' n j p; *)
            t.(i).(j) <- Some (res @ List.map (fun s -> j::s) (fill_t (i') (j+1)))
        end;
        Option.get t.(i).(j);
    in
    Array.init n (fun i -> fill_t i 0)
    


let find_best_presses (volts: int array) (buts: int list array): (int * int array) option =
    let all_presses_tab = all_presses (Array.length volts) buts in

    let get_all_presses (objective: bool array) =
        all_presses_tab.(objective |> Array.to_list |> r2) 
    in

    let rec solve (volts: int array) (buts: int list array) (lvl: int) : (int * int array) option =
        let n = Array.length buts in
        if Array.exists ((>) 0) volts then None else
        if Array.for_all ((=) 0) volts then begin
            (* Printf.printf "[%d]                   Found solution\n"; *)
            Some (0, Array.make n 0) 
        end else

        let objective = Array.map (fun v -> v mod 2 = 1) volts in
        (* Printf.printf "[%d] Current objective: " lvl; *)
        (* Array.iter (Printf.printf "%d ") volts; *)
        (* Printf.printf "\n"; *)

        let forks = get_all_presses objective in
        (* Printf.printf "[%d] Done forking:" lvl; *)
        (* List.iter (fun b -> List.iter (Printf.printf "%d ") b; Printf.printf ", ") forks; *)
        (* Printf.printf "\n"; *)

        if forks = [] then None else
        (* pour chaque liste de boutons bb, essayer de résoudre le réduit  *)
        let results = List.filter_map (fun (bb: int list) ->
            (* Printf.printf "[%d] Starting recursive on: " lvl; *)
            (* List.iter (Printf.printf "%d ") bb; *)
            (* Printf.printf "\n"; *)
            let new_volts = Array.copy volts in
            List.iter (fun i -> (List.iter (fun j -> new_volts.(j) <- new_volts.(j) - 1) buts.(i))) bb;
            let prev = solve (Array.map (fun x -> x / 2) new_volts) buts (lvl+1) in
            match prev with 
            | None -> None
            | Some (tot, sol) -> begin 
                let sol' = Array.copy sol |> Array.map (( * ) 2) in
                List.iter (fun j -> sol'.(j) <- sol'.(j) + 1) bb;
                Some (2*tot + List.length bb, sol')
            end
        )   
        forks
        in 
        if results = [] then None else
        Some(List.fold_left min (List.hd results) results)
    in
    solve volts buts 0


let parse_ligne (s: string) =
    let obj::[rest] = String.split_on_char ']' s in
    let obj = obj |> String.to_seq |> List.of_seq |> List.map ((=) '#') |> List.tl in
    let buts::[volts] = String.split_on_char '{' rest in
    let buts = 
        buts |> String.split_on_char ' ' |> List.tl
        |> List.rev |> List.tl |> List.rev 
        |> List.map (fun s -> String.sub s 1 (String.length s-2))
        |> List.map (String.split_on_char ',')
        |> List.map (List.map int_of_string)
    in
    let volts = 
        String.sub volts 0 (String.length volts-1)
        |> String.split_on_char ','
        |> List.map (int_of_string)
    in 
    obj |> Array.of_list, buts |> Array.of_list, volts |> Array.of_list    

let day_10 (data: string list) =
    data |> List.map parse_ligne
    |> List.map (fun (_, buts, volts) -> find_best_presses volts buts)
    |> List.map Option.get |> List.map fst
    |> List.fold_left (+) 0

let main () =
    let sol = if test then -1 else day_10 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()