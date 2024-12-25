
open Advent_of_code;;
let test = false
let data = get_input 2016 2

let grid = [|
    [|1;2;3|];
    [|4;5;6|];
    [|7;8;9|]
|]


let grid = [|
    [|'0'; '0'; '1'; '0'; '0'|];
    [|'0'; '2'; '3'; '4'; '0'|];
    [|'5'; '6'; '7'; '8'; '9'|];
    [|'0'; 'A'; 'B'; 'C'; '0'|];
    [|'0'; '0'; 'D'; '0'; '0'|];
|]




let move i j d =
    let di, dj = match d with 
    | 'U' -> (-1, 0)
    | 'D' -> (1, 0)
    | 'L' -> (0, -1)
    | 'R' -> (0, 1)
    | _ -> (-1, 0)
    in let ii, jj = i+di, j + dj in 
    if ii < 0 || ii > 4 || jj < 0 || jj > 4 || grid.(ii).(jj) == '0' then (i, j)
    else (ii, jj)

let button (s: string) (i: int) (j: int) =
    let ir, jr  = ref i, ref j in
    for k = 0 to String.length s - 1 do
        let ii, jj = move !ir !jr s.[k] in 
        ir := ii;    
        jr := jj
    done; (!ir, !jr)
    


let rec day_2_pos (data: string list) (i, j) =
    match data with 
    | [] -> ""
    | b :: q -> let (i, j) = button b i j in 
        let x = grid.(i).(j) in 
        String.make 1 x ^ day_2_pos q (i, j)

let day_2 data =  day_2_pos data (2, 0)

let main () =
    let sol = if test then "-1" else day_2 data in

    if true then begin
        print_string sol; print_newline ();
        write_solution_string sol
    end	else if sol = "0" then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()