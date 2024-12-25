
open Advent_of_code;;
let test = false
let data = get_input 2024 15

let test_data = String.split_on_char '\n'
"##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

let dir c = match c with 
    |'<' -> (0, -1)
    |'>' -> (0, 1)
    |'v' -> (1, 0)
    |'^' -> (-1, 0)
    | _ -> (0, 0)


(* return boolean indicating if box in (i, j) can be pushed in direction 
   di (up or down) *)
let rec can_push_v g (i, j) di =
    (* print_int i; print_string " "; print_int j; print_newline (); *)
    if g.(i).(j) = '#' then false else
    if g.(i).(j) = '.' then true else  
    if g.(i).(j) = ']' then can_push_v g (i, j-1) di else (* assume left half *)

    let push_left, push_right = can_push_v g (i+di, j) di,  can_push_v g (i+di, j+1) di in 
    push_left && push_right

(* push box (i, j) in direction di. can_push_v g (i, j) di must be true  *)
let rec push_v g (i, j) di =
(*     print_int i; print_string " "; 
    print_int j; print_string " ";
    print_char g.(i).(j); print_newline (); *)
    if g.(i).(j) = '#' then failwith "should not happen" else
    if g.(i).(j) = '.' then () else  
    if g.(i).(j) = ']' then push_v g (i, j-1) di else begin(* assume left half *)
        push_v g (i+di, j) di;
        push_v g (i+di, j+1) di;
        g.(i+di).(j) <- '[';
        g.(i+di).(j+1) <- ']';
        g.(i).(j) <- '.';
        g.(i).(j+1) <- '.'
    end

let push_h g (i, j) dj =
    let n, m = Array.length g, Array.length g.(0) in
    let moves = ref 0 in 
    while    0 <= j+(!moves+1)*dj && j+(!moves+1)*dj < m 
          && (g.(i).(j+(!moves+1)*dj) = '[' || g.(i).(j+(!moves+1)*dj) = ']') do
        incr moves;
    done;
    begin
        if 0 <= j+(!moves+1)*dj && j+(!moves+1)*dj < m && g.(i).(j+(!moves+1)*dj) = '.' then begin
            for k = !moves downto 0 do
               (*  print_int k; print_string " "; print_char g.(i).(j + (k+1) * dj);
                print_char g.(i).(j + k * dj); print_newline(); *)
                    g.(i).(j + (k+1) * dj) <- g.(i).(j + k * dj)
            done;
            (i, j+dj)
        end else
            (i, j)
    end

let push (g: char array array) (i, j) (di, dj) =
    if (di = 0 && dj = 0) then (i, j) else
    if dj = 0 then 
        let b = can_push_v g (i+di, j) di in 
        if b then begin
            push_v g (i+di, j) di; (i+di, j)
        end else (i, j)

    else 
        push_h g (i, j) dj


let print_grid g pos =
    let n, m = Array.length g, Array.length g.(0) in 
     for i = 0 to n-1 do
        for j = 0 to m-1 do
            if (i, j) = pos then print_char '@' else 
            print_char g.(i).(j)
        done;
        print_newline ()
    done

let rec split_data l =
    match l with 
    | "" :: q -> [], List.fold_left ( ^ ) "" q 
    | x::q -> let (lg, instr) = split_data q in x::lg, instr
    | [] -> ([], "")

let rec moves (i, j) g l = match l with 
    | [] -> ()
    | c :: q -> let (i, j) = push g (i, j) (dir c) in 
(*                 print_char c; print_newline ();
                print_grid g (i, j);
                Unix.sleep 1; *)
                moves (i, j) g q

let day_15 (data: string list) =
    let lg, instr = split_data data in 
    let instr = List.of_seq (String.to_seq instr) in

    let llg = List.map ( fun s -> List.of_seq (String.to_seq s) ) lg in 
    let rec double row = match row with 
        | [] -> []
        | 'O'::q -> '['::']'::double q
        | c::q when c != '@' -> c::c::double q
        | c::q -> c :: '.' :: double q
    in let llg = List.map double llg in 
    let lg = List.map (fun s -> Array.of_seq (List.to_seq s)) llg in
    let g = Array.of_list lg in 
    let n, m = Array.length g, Array.length g.(0) in
    let pos = ref (0, 0) in 
    for i = 0 to n-1 do
        for j = 0 to m-1 do
            if g.(i).(j) = '@' then begin
                pos := (i, j);
                g.(i).(j) <- '.'
            end
        done
    done;
    print_grid g !pos;
    moves !pos g instr;
    let res = ref 0 in 
    for i = 0 to n-1 do
        for j = 0 to m-1 do
            if g.(i).(j) = '[' then begin
                res :=  !res + 100 * i + j
            end
        done
    done;
    !res 

    

let main () =
    let sol = if test then -1 else day_15 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()