
open Advent_of_code;;
let test = false
let data = get_input 2024 19


let is_possible s l =
    let n = String.length s in
    let ways = Array.make (n+1) (-1) in 
    let rec possible_from (s: string) (l: string list) (i: int) : int=
        if ways.(i) >= 0 then ways.(i) else 
        let answer = 
            if i = String.length s then 1 else
            l |> 
            List.filter (fun t -> i+String.length t <= String.length s && String.sub s i (String.length t) = t) |> 
            List.map (fun t -> possible_from s l (i+String.length t)) |> 
            List.fold_left (+) 0 
            
        in ways.(i) <- answer; answer
    in possible_from s l 0
    

let day_19 (data: string list) =
    let patterns :: _ :: designs = data in 
    let patterns = 
        " " ^ patterns  |> 
        String.split_on_char ',' |> 
        List.map (fun s -> String.sub s 1 (String.length s - 1))
    in
    designs |> 
    List.map (fun s -> is_possible s patterns) |> 
    List.fold_left (+) 0


let main () =
    let sol = if test then -1 else day_19 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()