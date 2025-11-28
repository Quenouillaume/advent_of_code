
open Advent_of_code;;
let test = false
let data = get_input 2025 2

let day_2 (data: string list) =
    0

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