
open Advent_of_code;;
let test = false
let data = get_input 2024 14



let parse s = 
    let p::v::_ = String.split_on_char ' ' s in 
    
    let _::p::_ = String.split_on_char '=' p in  
    let px::py::_ = String.split_on_char ',' p in 

    let _::v::_ = String.split_on_char '=' v in  
    let vx::vy::_ = String.split_on_char ',' v in 
    int_of_string px, int_of_string py, (101 + int_of_string vx) mod 101, (103 + int_of_string vy) mod 103  


let pos_after n (x, y, vx, vy) = 
    (x + n * vx) mod 101,
    (y + n * vy) mod 103


let move_after n (x, y, vx, vy) = 
    (x + n * vx) mod 101,
    (y + n * vy) mod 103,
    vx, vy



let rec score (x, y, vx, vy) = 
    if abs(x-50) + abs(y-50) < 50 then 1 else 0

let rec quads l =
    match l with 
    | [] -> (0, 0, 0, 0)
    | (x, y) :: q ->
        let a,b,c,d = quads q in 
        let a = if x < 50 && y < 51 then a+1 else a in 
        let b = if x < 50 && y > 51 then b+1 else b in 
        let c = if x > 50 && y < 51 then c+1 else c in 
        let d = if x > 50 && y > 51 then d+1 else d in 
        (a, b, c, d)

(* part 2: wtf ??? *)
let move (x, y, vx, vy) = 
    let x, y = pos_after 1 (x, y, vx, vy) in 
    (x, y, vx, vy)

let show l =
    if (List.fold_left (+) 0 (List.map score l)) < 300 then () else begin
        let grid = Array.make_matrix 103 101 '.' in 
        let rec fill l = 
            List.iter (fun (x, y, vx, vy) -> grid.(y).(x) <- 'O') l
        in 
        fill l;
        for y = 0 to 102 do
            for x = 0 to 100 do
                print_char grid.(y).(x)
            done;
            print_newline ()
        done
    end
    



let day_14 (data: string list) =
    let n = 7672 in 
    let robots = data |> List.map parse |> List.map (move_after n) in 
    let rec toilets l i = 
        print_int i; print_newline ();
        print_int (List.fold_left (+) 0 (List.map score l));
        show l;
        print_newline (); print_newline ();
        print_newline (); print_newline ();

        (* Unix.sleep 1; *)
        ignore (Unix.select [] [] [] 0.05); (* usleep *) 
        if i < n+5000 then toilets (List.map move l) (i+1)
        else ()
    in toilets robots n; 0

let main () =
    let sol = if test then -1 else day_14 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()