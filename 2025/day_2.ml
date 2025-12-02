
open Advent_of_code;;
let test = false
let data = get_input 2025 2

let tdata=["11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"]


let is_doub n =
    let s = string_of_int n in 
    let n = String.length s in
    n mod 2 = 0 &&
    String.sub s 0 (n/2) = String.sub s (n/2) (n/2)


(* Part 2: u = a^i <=> exists v, w s.t. u = v.w = w.v *)

let is_invalid k =
    let s = string_of_int k in
    let r = ref false in
    let n = String.length s in
    let i = ref 2 in
    while !i <= n && not !r do
         if n mod !i = 0 &&
            String.sub s (n / !i) (n- n / !i) ^ String.sub s 0 (n / !i) = s then
            r := true;
        incr i
    done;
    !r
    


let range (x, y) =
    List.init (y-x+1) (fun i -> i + x)

let day_2 (data: string list) =
    data |> List.hd
    |> String.split_on_char ','
    |> List.map (String.split_on_char '-')
    |> List.map (fun (x::y::_) -> (int_of_string x, int_of_string y))
    |> List.map range |> List.map (List.filter is_invalid) |> List.concat
    |> List.fold_left (+) 0



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