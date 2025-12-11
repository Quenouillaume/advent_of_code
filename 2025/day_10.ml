
open Advent_of_code;;
let test = false
let data = get_input 2025 10

let start_time = Unix.time ()
let debug_print () =
    let t = int_of_float (Unix.time () -. start_time) in
    let hours = t  / 3600 in
    let minutes = (t - 3600 * hours) / 60 in
    let seconds = (t - 3600 * hours - 60 * minutes) in
    Printf.printf "[%3d:%02d:%02d] " hours minutes seconds;
    Printf.printf


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


(* Part 2: Gauss elimination ? *)
let gauss_elim (mat: int array array): int array =
    let n = Array.length mat in
    let m = Array.length mat.(0) in

    let show () =
        for i = 0 to n-1 do
            for j = 0 to m-1 do
                Printf.printf " %4d" mat.(i).(j)
            done;
            print_newline()
        done
    in

    (* Printf.printf "Starting pivot"; print_newline(); flush stdout; *)
    let r = ref (-1) in (* dernier pivot *)
    for j = 0 to m-2 do
        (* Printf.printf "Searching pivot for j=%d" j; print_newline(); flush stdout; *)
        (* show(); *)
        (* find largest element in column j *)
        let i_max = ref (!r+1) in
        for i = !r+2 to n-1 do
            if mat.(i).(j) >= mat.(!i_max).(j) && mat.(i).(m-1) > mat.(!i_max).(m-1) then
                i_max := i
        done;
        let i_max = !i_max in
        let piv = 
            if (0 <= i_max && i_max < n) then mat.(i_max).(j) 
            else 0
        in
        (* Printf.printf "Pivot for j=%d: %d" j i_max; print_newline(); flush stdout; *)

        if piv <> 0 then begin
            incr r;
            (* diviser ligne i_max par le pivot (inutile ici car c'est toujours 1 (ou pas en fait...)) *)
            for j' = 0 to m-1 do
                mat.(i_max).(j') <- mat.(i_max).(j')
            done;

            (* Échanger les lignes i_max et r *)
            if i_max <> !r then begin
                for j' = 0 to m-1 do
                    let tmp = mat.(i_max).(j') in
                    mat.(i_max).(j') <- mat.(!r).(j');
                    mat.(!r).(j') <- tmp
                done
            end;
            (* Printf.printf "After line swap:"; print_newline(); flush stdout; *)
            (* show(); *)
            (* soustraire à chaque ligne Li la qté Lr * mat[i][j] *)
            for i = 0 to n-1 do
                if  i <> !r then begin
                    let to_cancel = mat.(i).(j) in
                    for j' = 0 to m-1 do
                        mat.(i).(j') <- mat.(i).(j') - mat.(!r).(j') * to_cancel
                    done
                end
            done;
            (* Printf.printf "After Substracing row %d:" !r; print_newline(); flush stdout; *)
            (* show(); *)

        end
    done;
    (* Printf.printf "Done pivoting"; print_newline(); flush stdout; *)
    (* show(); *)


    (* reconstruct solution: for each row, find pivot column *)
    let x = Array.make (m-1) 0 in
    for i = 0 to n-1 do
        let j = ref 0 in
        while !j < m-1 && mat.(i).(!j) = 0  do
            (* Printf.printf "  R: %d %d" i !j; print_newline(); flush stdout; *)
            incr j
        done;
        if !j < m-1 then
            x.(!j) <- mat.(i).(m-1)
    done;
    (* Printf.printf "Done reconstructing"; print_newline(); flush stdout; *)
    x

let build_problem (buts: int list list) (volts: int list): int array array =
    (* print_string "Start building matrix"; print_newline(); flush stdout; *)
    let n = List.length volts in
    let m = List.length buts in
    let mat = Array.make_matrix n (m+1) 0 in
    List.iteri (fun j b -> List.iter (fun i -> mat.(i).(j) <- 1) b) buts;
    List.iteri (fun i v -> mat.(i).(m) <- v) volts;
    (* Printf.printf "Built matrix %d x %d" n m; print_newline(); flush stdout; *)
    mat



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
    obj, buts, volts    

let pre_solve_part2 buts volts =
    let mat = build_problem buts volts in
    let x = gauss_elim mat in
    if Array.exists ((<) 0) x then None else begin
        let res = ref 0 in
        Array.iter (fun k -> res := !res + abs k) x;
        Some !res
    end

(* Gauss doesn't necessarily give the optimal solution :( *)

(* PART2: backtracking ? *)

exception FOUND of int
(* how many presses to reduce volts to zero using buttons from buts.
   links: list of pairs (i, i') where counter i must be less than counter i'
   indeps: list of pairs (i, i') where counters i and i' cannot be activated with a single button
   activators: how many buttons can activate each counter
   curr = current number of buttons pressed
   curr_best = best solution value found.
   buts is sorted according to each button's max counter *)
let elim_active = ref 0
let elim_weak = ref 0
let elim_lb = ref 0
let elim_indep = ref 0
let elim_sum = ref 0
let init_max_voltage = ref 0
let rec best_presses (volts: int array) 
    (buts: int list list) 
    (links: (int*int) list) 
    (indeps: (int*int) list) 
    (activators: int array)
    (xors: (int list * int list) list)
    (total_voltage: int)
    (curr: int) (curr_best: int) : int =
    let n = Array.length volts in

   (* Stop searching if curr >= curr_best or if impossible to solve in less than curr_best-curr *)
    if curr >= curr_best then curr_best else
    if Array.for_all ((=) 0) volts then begin
        (debug_print ()) "Found solution: %d" curr; print_newline();
        if curr = !init_max_voltage then begin
            (debug_print ()) "SHORTCUT: %d" curr; print_newline();
            raise (FOUND curr) 
        end else
            curr 
    end else 

    (* Check if any link cannot be validated *)
    if List.exists (fun (i, i') -> volts.(i) > volts.(i')) links then begin
        (* Printf.printf "Elim weak"; print_newline(); *)
        incr elim_weak;
        curr_best
    end else

    (* lower bound on pairs of indeps *) 
    if List.exists (fun (i, i') -> curr + volts.(i) + volts.(i') >= curr_best) indeps then begin
        (* Printf.printf "Elim weak"; print_newline(); *)
        incr elim_indep;
        curr_best
    end else 

    (* Remove buttons which activate zero'd counters *)
    let new_voltage = ref total_voltage in
        let press_button b' k = 
            new_voltage := !new_voltage - k * List.length b';
            List.iter (fun j -> begin 
                volts.(j) <- volts.(j) - k;
                activators.(j) <- activators.(j) - 1;
            end) b'
        in
        let unpress_button b' k =
            new_voltage := !new_voltage + k * List.length b';
            List.iter (fun j -> begin 
                volts.(j) <- volts.(j) + k;
                activators.(j) <- activators.(j) + 1;
            end) b'
        in
    (* Abort if non-zero counter cannot be activated,
       find potential single-button counter,
       and detect buttons which activate a zero'd counter *)
    let single = ref (-1) in
    let possible = ref true in
    let zeroed_out = ref [] in
    for j = 0 to Array.length volts - 1 do
        if volts.(j) > 0 && activators.(j) = 0 then possible := false;
        if volts.(j) = 0 && activators.(j) > 0 then begin
            zeroed_out := !zeroed_out @ List.filter (List.mem j) buts
        end;
        if activators.(j) = 1 && volts.(j) > 0 then begin
            assert (List.exists (List.mem j) buts);
            single := j;
        end
    done;
    let zero_buts = [] in 
    (* let zero_buts = List.sort_uniq compare (!zeroed_out) in *)
    let buts = List.filter (fun b -> not (List.mem b zero_buts)) buts in
    if buts = [] then begin 
        curr_best
    end else let zfgd = 0 in
    List.iter (Fun.flip press_button 0) zero_buts;



    if not !possible then begin
        (* Printf.printf "Elim activators"; print_newline(); *)
        incr elim_active;
        List.iter (Fun.flip unpress_button 0) zero_buts;
        curr_best 
    end else 


    (* Stop searching if a counter i only has activators of size >= A
       s.t. A * volts.(i) > total_voltage: a solution would require
       at least volts.(i) presses of a button activating i, which would
       incur a negative total voltage *)
    let stop = ref false in
    for i = 0 to n-1 do
        let small_but = List.filter (List.mem i) buts |> List.map List.length |> List.fold_left min n in
        if volts.(i) * small_but > total_voltage then stop := true
    done;
    if !stop then begin
        List.iter (Fun.flip unpress_button 0) zero_buts;
        curr_best 
    end else

    (* Determine max amount of presses possible for button b *)
    let max_presses b = 
        let basic =
            b
            |> List.map (fun j -> volts.(j)) 
            |> List.fold_left min (volts.(List.hd b)) 
        in
        let advanced = (* for any (i, i') in links, if i' is in b and not i then
          cannot press b more than volts.(i') - volts.(i) times *)
          links |> List.filter (fun (i, i') -> List.mem i' b && not (List.mem i b))
          |> List.map (fun (i, i') -> volts.(i') - volts.(i))
          |> List.fold_left min basic
        in
        advanced
    in
(*     let buts = List.sort
        (fun b1 b2 -> 2 * Random.int 2 - 1) 
        buts
    in *)

    match buts with
    | [] -> begin List.iter (Fun.flip unpress_button 0) zero_buts; curr_best end
    | b :: bs -> begin
        assert( b <> []);

    (* lower bound if largest button is small *)
    let butt_size = List.fold_left max (List.length b) (List.map List.length bs)  in
    (* let butt_size = List.length b in *)
    let lower_bound = (total_voltage-1) / butt_size + 1 + curr in
    if (lower_bound > curr_best) then begin
        incr elim_sum;
        List.iter (Fun.flip unpress_button 0) zero_buts;
        curr_best
    end else begin
    
(*      Printf.printf "solving "; Array.iter (fun x -> Printf.printf "%d " x) volts;
        Printf.printf " using button "; List.iter (fun x -> Printf.printf "%d " x) b; print_newline(); flush stdout;  *)

        (* if a counter is single-button activated, then choose it instead *)
        let b = if !single >= 0 then begin
            assert (List.filter (List.mem !single) buts <> []);
            List.hd (List.filter (List.mem !single) buts)
        end else b
        in 
        let bs = if !single >= 0 then
            List.filter ((<>) b) buts
        else bs
        in
        let maxp = max_presses b in
        let minp = if !single >= 0 then maxp else 0 in


        (* try all amounts *)
        let res = ref curr_best in
        (* for k = minp to maxp do *)
        for k = maxp downto minp do
            (* press b k times then solve recursively *)
            let new_voltage = total_voltage - k * List.length b in
            List.iter (fun j -> begin 
                volts.(j) <- volts.(j) - k;
                activators.(j) <- activators.(j) - 1;
            end) b;

            (* if chosen button is in a xor, remove other button *)
            let removed, bs = if k = 0 then [], bs else bs |>
                List.partition (fun b' -> List.mem (b', b) xors)
            in
            List.iter (Fun.flip press_button 0) removed;

            let y = best_presses 
                volts 
                bs 
                (links) 
                (indeps) 
                (activators) 
                (xors)
                (new_voltage) 
                (curr+k) !res 
            in
            if y < !res then res := y;
            (* Restore initial state *)
            List.iter (Fun.flip unpress_button 0) removed;

            List.iter (fun j -> begin 
                volts.(j) <- volts.(j) + k;
                activators.(j) <- activators.(j) + 1;
            end) b;
        done;
        List.iter (Fun.flip unpress_button 0) zero_buts;
        !res
    end
    end

let buttons_per_counter buts volts =
    let n = Array.length volts in
    let res = Array.make n [] in
    List.iteri (fun j b -> List.iter (fun i -> res.(i) <- j :: res.(i)) b) buts;
    res

let reduce (buts: int list list ref) (volts: int array) : int =
    let n = Array.length volts in
    let bpc = buttons_per_counter !buts volts in
    Array.iteri (fun j l ->
        (debug_print ()) "Activators of %d: " j;
        List.iter (Printf.printf "%d ") l;
        print_newline();
    ) bpc;
    let j_reduce = ref None in
    let i = ref 0 in
    let presses = ref 0 in
    while !j_reduce = None && !i < n do
        if volts.(!i) > 0 then begin 
        match bpc.(!i) with
        | [j] -> (* only button j activates i *) begin
            (debug_print ()) "Button %d can be removed: only way to touch %d\n" j !i; flush stdout;
            let b = List.nth !buts j in
            let new_presses = volts.(!i) in
            presses := !presses + new_presses;
            List.iter (fun i' -> volts.(i') <- volts.(i') - new_presses) b;
            buts := List.filter ((<>) b) !buts;
            j_reduce := Some j 
        end
        | _ -> ();
    end;
    incr i;
    done;
    !presses


let reduce2 (buts: int list list) (volts: int array) : (int* int) list =
    let n = Array.length volts in
    let bpc = buttons_per_counter buts volts in
(*     Array.iteri (fun j l ->
        Printf.printf "Activators of %d: " j;
        List.iter (Printf.printf "%d ") l;
        print_newline();
    ) bpc; *)
    let res = ref [] in
    for i = 0 to n-1 do
        for i' = 0 to n-1 do
            if i <> i' && bpc.(i) <> [] && List.for_all (Fun.flip List.mem bpc.(i')) bpc.(i) then begin
                (* counter i  is useless: any hit to i also hits i' *)
                (debug_print ()) "counter %d is weaker than %d" i i'; print_newline();
                res := (i, i') :: !res;
                
            end
        done
    done;
    !res
    

let prod l1 l2 =
    l1 |> List.map (fun x -> List.map (fun y -> (x, y)) l2 |> List.filter (fun (x, y) -> x < y)) |> List.concat

let xor_buts (buts: int list list): (int list * int list) list =
(* return list of pairs of buttons (b1, b2) such that 
   b1 and b2 cannot both be used in a solution
   because b1 |_| b2 = another button b3 *)
   prod buts buts
   |> List.filter (fun (b1, b2) -> 
        not (List.exists (Fun.flip List.mem b1) b2) && 
        List.exists (fun b3 -> 
            List.sort_uniq compare (b1 @ b2) = List.sort_uniq compare b3
        ) buts
    )




let solve_part2 (s: string) =

    let _, buts, volts = parse_ligne s in

    (* solution must use less buttons than sum of voltages *)
    let bound1 = List.fold_left (+) 0 volts in 
    (* solution must use less buttons than sum of voltages divided by smallest button
       e.g. if all buttons are of size 3, then k presses add 3k to total voltage *)
    let bound2 = 1 + (bound1-1) / (buts |> List.map List.length |> List.fold_left min (List.length volts)) in

    let volts = Array.of_list volts in

    (* Reduce input by removing counters that can only be
       activating by a single button *)
    let pre_presses = ref 0 in
    let buts = ref buts in
    let keep_reducing = ref true in
    while (!keep_reducing) do
        let presses = reduce buts volts in
        if presses = 0  then keep_reducing := false;
        pre_presses := !pre_presses + presses;
        (debug_print ()) "After reduce:";
        Array.iter (Printf.printf "%d ") volts; print_newline();
    done;

    if Array.for_all ((=) 0) volts then begin
        (debug_print ()) "Reduced solution: %d" !pre_presses; print_newline();
        !pre_presses 
    end else begin
        let buts = !buts in

        (* Sort buttons by decreasing size*)
        let buts = List.sort (fun b b' -> compare (List.length b') (List.length b)) buts in
        (* List.iter (fun n -> print_int n; print_newline()) (List.map List.length buts); *)        (* Get pairs of linked counters *)
        let links = reduce2 buts volts in

        (* Get total voltage *)
        let total_voltage = List.fold_left (+) 0 (Array.to_list volts) in

        (* Get independent pairs *)
        let n = Array.length volts in
        let indeps = 
            List.init n (Fun.id) 
            |> List.map (fun i -> 
                List.init n (Fun.id) 
                |> List.filter (fun j -> List.for_all (fun b -> not (List.mem i b && List.mem j b)) buts) 
                |> List.map (fun j -> (min i j, max i j))
            )
            |> List.concat
            |> List.sort_uniq compare
        in
        (debug_print ()) "Independents: "; List.iter (fun (i, i') -> Printf.printf "(%d, %d) " i i') indeps; print_newline();


        (* Count activators *)
        let activators = Array.make n 0 in
        List.iter (List.iter (fun i -> activators.(i) <- activators.(i) + 1)) buts;

        (* Get xors *)
        let xors = xor_buts buts in
        (debug_print ()) "Xors found: %d" (List.length xors); print_newline();

        (* Compute upper bounds *)
            (* solution must use less buttons than sum of voltages *)
        let bound1 = total_voltage in
        (* solution must use less buttons than sum of voltages divided by smallest button
           e.g. if all buttons are of size 3, then k presses add 3k to total voltage *)
        let bound2 = 1 + (bound1-1) / (buts |> List.map List.length |> List.fold_left min (Array.length volts)) in

        let bound = bound2 in

        elim_weak := 0;
        elim_active := 0;
        elim_lb := 0;
        elim_indep := 0;
        elim_sum := 0;
        init_max_voltage := List.fold_left (max) (volts.(0)) (Array.to_list volts);
        let res = 
            try best_presses volts buts links indeps activators xors total_voltage 0 bound 
            with | FOUND x -> x
        in
        (debug_print ()) "Solution: %d (elim: %d + %d + %d + %d + %d)" res 
            !elim_weak !elim_active !elim_lb !elim_indep !elim_sum; 
        print_newline();

        res + !pre_presses
    end

let day_10 (data: string list) =
    data |> List.map solve_part2
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