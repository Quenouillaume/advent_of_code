open Advent_of_code
let test = false
let data = get_input 2025 1


let rec sum_rot l p =
    match l with 
    | [] -> 0
    | (b, x) :: q -> sum_rot q ((((p + (if b then x else -x)) mod 100) + 100) mod 100) + 
        if b && p + x >= 100 then (p+x) / 100 
        else if not b && p - x <= 0 then (if p > 0 then 1 else 0) + (-p+x)/100
        else 0

let day_1 (data: string list) =
    let l = data 
    |> List.map (fun s -> ((s.[0]  = 'R'), String.sub s 1 (String.length s - 1)))
    |> List.map (fun (b, s) -> (b, int_of_string s))
    in sum_rot l 50

(* let _ = List.iter (fun s -> print_string s; print_string "\n") data *)

let main () =
    let sol = if test then -1 else day_1 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()