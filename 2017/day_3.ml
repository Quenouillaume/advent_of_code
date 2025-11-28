
open Advent_of_code;;
let test = false
let data = get_input 2017 3



(** Partie 1: O(1) **)
let f n = 1 + 4 * n * (n+1)

let ring2 x =
    int_of_float (ceil ((sqrt (float_of_int x) +. 1.) /. 2.)) -1

let steps x =
    if x = 1 then 0 else
    let n = ring2 x in
    abs(n-1-((x - f (n-1) - 1) mod (2*n))) + n



(** Partie 2: https://oeis.org/A141481 **)


let day_3 (data: string list) =
    data |> List.hd |> int_of_string |> steps

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