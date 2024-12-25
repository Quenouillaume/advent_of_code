
open Advent_of_code;;
let test = false
let data = get_input 2016 3


let parse (s: string) : (int * int * int) = 
    match List.map int_of_string (List.filter (fun s -> s <> "") (String.split_on_char ' ' s)) with
    | x::y::z::q -> (x, y, z)
    | _ -> print_string "Error\n"; (0, 0, 0)    


let is_triangle (x,y,z) = 
    if x + y > z && x + z > y && y + z > x then (
        begin 
            print_int x; print_string " ";
            print_int y; print_string " ";
            print_int z; print_string "\n"
        end; true)
    else 
        false

let three_tris (t1, t2, t3) =
    let x1, y1, z1 = t1 in 
    let x2, y2, z2 = t2 in 
    let x3, y3, z3 = t3 in 
    [x1, x2, x3 ; y1, y2, y3; z1, z2, z3] 

let day_3 (data: string list) =
    let rec reorg l = match l with 
    | [] -> []
    | t1::t2::t3::q -> let ts = three_tris (t1, t2, t3) in ts @ reorg q
    | _ -> failwith "pas assez :("
    in

    let l = reorg (List.map parse data) in 

    List.fold_left (fun x b -> if b then x+1 else x) 0 (List.map (fun t -> is_triangle t) l)


let main () =
    let sol = if test then -1 else day_3 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()