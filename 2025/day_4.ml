
open Advent_of_code;;
let test = false
let data = get_input 2025 4

let tdata = String.split_on_char '\n'
"..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."


let is_accessible g i j =
    let n = Array.length g in
    let m = Array.length g.(0) in
    if not g.(i).(j) then false else
    let neighbrs = ref 0 in
    for di = -1 to 1 do
        for dj = -1 to 1 do
            if di <> 0 || dj <> 0 then
                let ii, jj = i + di, j + dj in
                if 0 <= ii && ii < n && 0 <= jj && jj < m && g.(ii).(jj) then
                    incr neighbrs
        done
    done;
    !neighbrs < 4
    

let count_access (g: bool array array) : int =
    let n = Array.length g in
    let m = Array.length g.(0) in
    let res = ref 0 in
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            if is_accessible g i j then incr res       
        done
    done;
    !res
    


(* PART 2 in O(n^2 m^2) ... *)
let rec stable g =
    let n = Array.length g in
    let m = Array.length g.(0) in
    let res = ref 0 in
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            if is_accessible g i j then begin 
                incr res;
                g.(i).(j) <- false
            end       
        done
    done;
    !res + (if !res = 0 then 0 else stable g)


(* PART 2 faster: O(nm) with topological sort :D *)

let add_roll g deg i j =
    let n = Array.length g in
    let m = Array.length g.(0) in
    if not g.(i).(j) then () else
    for di = -1 to 1 do
        for dj = -1 to 1 do
            if di <> 0 || dj <> 0 then
                let ii, jj = i + di, j + dj in
                if 0 <= ii && ii < n && 0 <= jj && jj < m && g.(ii).(jj) then
                    deg.(ii).(jj) <- deg.(ii).(jj) + 1
        done
    done


let rec topo g =
    let n = Array.length g in
    let m = Array.length g.(0) in
    (* deg.(i).(j) = number of neighbouring rolls remaining *)
    let deg = Array.make_matrix n m 0 in
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            add_roll g deg i j                
        done
    done;
    (* todo: list of detected accessible rolls *)
    let todo = ref [] in
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            if g.(i).(j) && deg.(i).(j) < 4 then
                todo := (i, j) :: !todo
        done
    done;

    (* topological sort *)
    let res = ref 0 in
    while !todo <> [] do
        let ((i, j) :: q) = !todo in
        todo := q;
        incr res;
        (* update todo list: new accessible rolls have exactly 3 neighbours *)
        for di = -1 to 1 do
            for dj = -1 to 1 do
                if di <> 0 || dj <> 0 then begin
                    let ii, jj = i + di, j + dj in
                    if 0 <= ii && ii < n && 0 <= jj && jj < m && g.(ii).(jj) then begin
                        deg.(ii).(jj) <- deg.(ii).(jj) - 1;
                        if deg.(ii).(jj) = 3 then
                            todo := (ii, jj) :: !todo
                    end
                end
            done
        done
    done;
    !res


let day_4 (data: string list) =
    data |> List.map (fun s -> s |> String.to_seq |> Array.of_seq) |> Array.of_list
    |> Array.map (Array.map ((=) '@'))
    |> topo



let main () =
    let sol = if test then -1 else day_4 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()