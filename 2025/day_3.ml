
open Advent_of_code;;
let test = false
let data = get_input 2025 3

let tdata = String.split_on_char '\n'
"987654321111111
811111111111119
234234234234278
818181911112111"

let maxl (x::q) =
    List.fold_left max x q

let rec jolt_max l = 
    match l with 
    | x :: y :: [] -> (10*x + y, max x y)
    | x :: q -> let j, m = jolt_max q in (max j (10*x+m), max x m)



(* Part 2: Dynamic programming ? *)
let rec pow x n = if n = 0 then 1 else pow (x*x) (n/2) * (if n mod 2 = 1 then x else 1)

let rec n_jolt_max (t: int array) (k: int) =
    let n = Array.length t in
    let jolt = Array.make_matrix n (k+1) (-1) in 
    (* jolt[i][j] = max joltage from t[i] using j batteries *)
    for i = 0 to n-1 do
        for j = n-i+1 to min n k do
           jolt.(i).(j) <- 0
        done
    done;
    let rec calc_jolt i j : int =
        (* print_int i; print_string "/"; print_int j; print_newline(); *)
        if i >= n || j <= 0 then 0 else begin
        if jolt.(i).(j) = -1 then begin
            (* pick batt i or not *)
            jolt.(i).(j) <- max (pow 10 (j-1)*t.(i) + calc_jolt (i+1) (j-1)) (calc_jolt (i+1) (j))
        end;
        jolt.(i).(j)
        end
    in calc_jolt 0 k

let day_3 (data: string list) =
    data |> List.map String.to_seq |> List.map List.of_seq
    |> List.map (List.map int_of_char) 
    |> List.map (List.map (Fun.flip (-) (int_of_char '0')))
    |> List.map (Array.of_list)
    |> List.map (Fun.flip n_jolt_max 12)
    |> List.fold_left (+) 0

let main () =
    let sol = if test then -1 else day_3 tdata in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()