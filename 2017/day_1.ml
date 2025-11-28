
open Advent_of_code;;
let test = false
let data = get_input 2017 1


let match_sum (s: string) : int =
    let res = ref 0 in
    let n = String.length s in
    for i = 0 to n-1 do
        if s.[i] == s.[(i+1) mod n] then
            res := !res + (int_of_char s.[i] - int_of_char '0')        
    done;
    !res

let match_sum_circ (s: string) : int =
    let res = ref 0 in
    let n = String.length s in
    for i = 0 to n-1 do
        if s.[i] == s.[(i+n/2) mod n] then
            res := !res + (int_of_char s.[i] - int_of_char '0')        
    done;
    !res
    


let day_1 (data: string list) =
    data |> List.hd |> match_sum

let day_2 (data: string list) =
    data |> List.hd |> match_sum_circ

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