
open Advent_of_code;;
let test = false
let data = get_input 2024 20

let tdata = String.split_on_char '\n' 
"###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"
  



let dirs = [(0, 1); (0, -1); (1, 0); (-1, 0)]  

let dists g (i0, j0) =
    let n, m = Array.length g, Array.length g.(0) in
    let dist = Array.make_matrix n m (-1) in 
    let seen i j = (dist.(i).(j) >= 0) in

    let nbrs (i, j) =
        dirs |> List.map (fun (di, dj) -> i + di, j + dj)
             |> List.filter (fun (ii, jj) -> 
                0 <= ii && ii < n && 0 <= jj && jj < m &&
                (g.(i).(j) = '.' && g.(ii).(jj) = '.'))
             |> List.filter (fun (ii, jj) -> 
                if not (seen ii jj) then begin 
                    dist.(ii).(jj) <- dist.(i).(j) + 1;
                    true
                end else false 
                )
    in
    let rec bfs l = match l with 
        | [] -> ()
        | _ -> l |> List.map nbrs |> List.concat |> bfs
    in
    dist.(i0).(j0) <- 0;
    bfs [(i0, j0)];
    dist

let day_20 (data: string list) =
    let g = data |> List.map String.to_seq |> List.map Array.of_seq
                    |> Array.of_list 
    in
    let n, m = Array.length g, Array.length g.(0) in
    let (is, js) = ref 0, ref 0 in 
    let ie, je = ref 0, ref 0 in 
    for i = 0 to n-1 do
        for j = 0 to m-1 do
            if g.(i).(j) = 'S' then begin 
                is := i; js := j; g.(i).(j) <- '.'
            end;
            if g.(i).(j) = 'E' then begin 
                ie := i; je := j; g.(i).(j) <- '.'
            end
        done
    done;
    let dist_e = dists g (!ie, !je) in 
    let dist_s = dists g (!is, !js) in 

    let no_cheat =  dist_e.(!is).(!js) in

    let is_good_cheat (i1, j1) (i2, j2) =
        if not (0 <= i2 && i2 < n && 0 <= j2 && j2 < m) then false else
        if dist_e.(i1).(j1) < 0 || dist_s.(i2).(j2) < 0 then false else
        if abs(i1 - i2) + abs(j1-j2) > 20 then false else
        (* if g.(i1).(j1) = '#' || g.(i2).(j2) = '#' then false else *)
        dist_e.(i1).(j1) + abs(i1 - i2) + abs(j1-j2) + dist_s.(i2).(j2) 
        <= no_cheat - 100

   in
(*     let cheats_around i j = 
        let cells = 
            dirs |> List.map (fun (di, dj) -> i + di, j + dj)
                 |> List.filter (fun (ii, jj) -> 
                    0 <= ii && ii < n && 0 <= jj && jj < m &&
                    g.(ii).(jj) = '.')
        in 
        let rec good_cheat l1 l2 = match l1, l2 with 
            | [], _ -> 0
            |  x::q, [] -> good_cheat q cells
            | (i1, j1)::q1, (i2, j2) :: q2 ->
              (if dist_e.(i1).(j1) + 2 + dist_s.(i2).(j2) <= no_cheat - 100 then begin
                print_int i1; print_string " ";
                print_int j1; print_string " ";
                print_int i2; print_string " ";
                print_int j2; print_string " ";
                print_int (dist_e.(i1).(j1) + 2 + dist_s.(i2).(j2)); print_string " ";
                print_int no_cheat; print_newline() ;
                1 
              end else 0)
              + good_cheat l1 q2
        in 
        good_cheat cells cells
                         
    in *)
    let res = ref 0 in 
    for i1 = 0 to n-1 do
        for j1 = 0 to m-1 do
            for di = -20 to 20 do
                for dj = - (20 - abs di) to (20 - abs di) do
                    if is_good_cheat (i1, j1) (i1+di, j1+dj) then
                        incr res
                done 
            done
        done 
    done;
    !res
    

let main () =
    let sol = if test then -1 else day_20 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()