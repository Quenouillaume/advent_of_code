
open Advent_of_code;;
let test = false
let data = get_input 2017 2

let rec max_l = function
    | [x] -> x
    | x :: q -> max x (max_l q) 

let rec min_l = function
    | [x] -> x
    | x :: q -> min x (min_l q) 


let even_div l =
    let rec div_by x = function 
        | [] -> None 
        | y :: q -> if y mod x = 0 && y <> x then Some (y/x) else div_by x q
     in
     List.map (Fun.flip div_by l) l |> List.filter_map Fun.id |> List.hd 

let day_2 (data: string list) =
    data |> List.map (String.split_on_char '\t')
    |> List.map (List.map int_of_string) 
    |> List.map (even_div)
    |> (fun l -> List.fold_left (+) 0 l) 

let main () =
    let sol = if test then -1 else day_2 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()