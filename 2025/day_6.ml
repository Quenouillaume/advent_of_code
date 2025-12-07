
open Advent_of_code;;
let test = false
let data = try get_input 2025 6 with | _ -> []

let expected = 3263827
let tdata = String.split_on_char '\n'
"123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  "

let subtotal nums ops j =
    let n = Array.length nums in
    let def, op = ops.(j) in
    let res = ref def in
    for i = 0 to n-1 do
        res := op !res nums.(i).(j)
    done;
    !res
    

let total nums ops =
    let m = Array.length ops in
    let res = ref 0 in
    for j = 0 to m-1 do
        res := !res + subtotal nums ops j
    done;
    !res


(* PART 2: encore des chiffres.... *)
let get_op_pos ops =
    let n = String.length ops in
    let rec get_from i =
        if i >= n then [n+1] else
        if ops.[i] <> ' ' then i :: get_from (i+1)
        else get_from (i+1)
    in get_from 0

let read_nums (nums: char array array) (op_pos: int) (next_op_pos: int) =
    let n = Array.length nums in (* number of rows *)
    let k = next_op_pos - op_pos - 1 in (* number of numbers to read *)
    let res = Array.make k 0 in
    for j = 0 to k-1 do
         (* read kth column *)
         for i = 0 to n-1 do
             if nums.(i).(op_pos+j) <> ' ' then
                 res.(j) <- 10 * res.(j) + int_of_char nums.(i).(op_pos+j) - int_of_char '0'
         done;
     done;
     res

let vert_total nums ops =
    let op_positions = get_op_pos ops |> Array.of_list in
    let p = Array.length op_positions in
    let total = ref 0 in
    for i = 0 to p-2 do
        let def, op = if ops.[op_positions.(i)] = '*' then (1, ( * )) else (0, ( + )) in
        let vert_nums = read_nums nums op_positions.(i) op_positions.(i+1) in
        let k = Array.length vert_nums in
        let res = ref def in
        for j = 0 to k-1 do
            res := op !res vert_nums.(j)
        done;
        total := !total + !res
    done;
    !total
    
      
(* PART 2 in pure functional *)
let rec transpose (ll: char list list) : char list list =
    match ll with 
    | [] -> []
    | [] :: _ -> []
    | _ -> (List.map List.hd ll) :: transpose (List.map List.tl ll)

(* read l in reverse as an int, ignoring spaces.  *)
let read_space_int (l: char list) : int option = 
    let s = l |> List.rev |> List.filter (fun c -> c <> ' ') |> List.to_seq |> String.of_seq in
    if s = "" then None else Some (int_of_string s)

(* Apply operator curr_op on curr list of numbers and on numbers read in l,
   then keep computing operations in l *)
let rec do_op (curr: int list) (curr_op: int * (int -> int -> int))  (l: char list list): int list =
    match l with
    | (c :: num) :: q when c <> ' ' ->
        do_op [Option.get (read_space_int num)] (if c ='*' then (1, ( * )) else (0, (+))) q
    | (num) :: q -> begin 
        match read_space_int num with 
        | None -> List.fold_left (snd curr_op) (fst curr_op) curr :: do_op [] curr_op q
        | Some k -> do_op (k::curr) curr_op q
        end
    | [] -> List.fold_left (snd curr_op) (fst curr_op) curr :: []
    
let day_6_fun data =
    data |> List.map String.to_seq |> List.map List.of_seq
    |> transpose
    |> List.map (List.rev)
    |> do_op [] (0, (+)) |> List.fold_left (+) 0
 


    

let day_6 (data: string list) =
    let ops :: nums = List.rev data in
    let nums = nums |> List.rev |> List.map (String.to_seq) |> List.map (Array.of_seq)
    |> Array.of_list
    in
    vert_total nums ops

let main () =
    if day_6_fun tdata <> expected then begin
        print_endline "Wrong answer on test";
        exit 1
    end else begin
        let sol = day_6 data in 
        print_int sol; print_newline();
        write_solution sol;
    end

let _ = main ()