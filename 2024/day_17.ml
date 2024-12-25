
open Advent_of_code;;
let test = false
let data = get_input 2024 17



let parse_reg s =
    match String.split_on_char ' ' s with 
    | _ :: _ :: v :: _ -> int_of_string v 
    | _ -> failwith "reg"

let parse_prog s = 
    match String.split_on_char ' ' s with 
    | _::p::_ -> 
        List.map int_of_string (String.split_on_char ',' p)
    | _ -> failwith "prog"

let parse data = 
    match data with 
    | a :: b :: c :: _ :: p :: _ ->
        (parse_reg a, parse_reg b, parse_reg c), Array.of_list (parse_prog p)
    | _ -> failwith "data" 

type op = 
    | Adv (* division: A <- A / 2**combo *) 
    | Bdv (* B <- A / 2**combo *)
    | Cdv (* C <- A / 2**combo *)
    | Bxl (* B <- B xor literal *) 
    | Bst (* B <- combo mod 8 *) 
    | Jnz (* if A is not 0, PC <- litteral *)
    | Bxc (* B <- B xor C *) 
    | Out (* output combo mod 8 *) 


let desc op = "[" ^ (match op with 
    | Adv -> "A <- A / 2**combo"
    | Bdv -> "B <- A / 2**combo"
    | Cdv -> "C <- A / 2**combo"
    | Bxl -> "B <- B xor literal" 
    | Bst -> "B <- combo mod 8" 
    | Jnz -> "PC <- litteral if A <> 0"
    | Bxc -> "B <- B xor C" 
    | Out -> "output combo mod 8") ^"]"

let op_of_code (c: int) = match c with 
    | 0 -> Adv
    | 1 -> Bxl
    | 2 -> Bst
    | 3 -> Jnz
    | 4 -> Bxc
    | 5 -> Out
    | 6 -> Bdv
    | 7 -> Cdv
    | _ -> failwith "code"

type state = (int*int*int) (* A, B, C *)

let get_combo (operand: int) ((a, b, c) : state) =
    match operand with 
    | 4 -> a 
    | 5 -> b 
    | 6 -> c 
    | 7 -> failwith "no"
    | _ -> operand

let rec pow x n = match n with 
    | 0 -> 1 
    | _ when n mod 2 = 0 -> pow (x*x) (n/2)
    | _ -> x * pow (x*x) (n/2)

let exec 
    (code: int) 
    (operand: int) 
    ((a, b, c) : state) : 
    state * int option * int option = (* new state, PC, output *)
    print_string "Executing "; print_string (desc (op_of_code code));
    print_string " on "; print_int operand;
    print_string " with registers ";
    print_int a; print_string " ";
    print_int b; print_string " ";
    print_int c; print_string "\n";
    let combo = get_combo operand (a, b, c) in 
    match op_of_code code with 
    | Adv -> let a = a / pow 2 combo in (a, b, c), None, None (* division: A <- A / (2**combo) *) 
    | Bdv -> let b = a / pow 2 combo in (a, b, c), None, None (* B <- A / combo *)
    | Cdv -> let c = a / pow 2 combo in (a, b, c), None, None (* C <- A / combo *)
    | Bxl -> let b = b lxor operand in (a, b, c), None, None(* B <- B xor literal *) 
    | Bst -> let b = combo mod 8 in (a, b, c), None, None (* B <- combo mod 8 *) 
    | Jnz -> (a, b, c), (if a != 0 then Some operand else None), None (* if A is not 0, PC <- litteral *)
    | Bxc -> let b = b lxor c in (a, b, c), None, None (* B <- B xor C *) 
    | Out -> (a, b, c), None, Some (combo mod 8) (* output combo mod 8 *) 

let program_full (p: int array) (s : state) =
    let n = Array.length p in 
    let rec program (p: int array) (pc: int) (s : state) : int list =
        let (a, b, c) = s in
        if pc >= n - 1 then begin
            print_int a; print_string " ";
            print_int b; print_string " ";
            print_int c; print_string "\n";
            [] 
        end else
        let s, pc_opt, out_opt = exec p.(pc) p.(pc+1) s in 
        let pc = match pc_opt with 
        | None -> pc + 2
        | Some pc' -> pc'
        in match out_opt with 
        | None -> program p pc s
        | Some out -> begin print_string "Output "; print_int out; print_newline (); out :: program p pc s end
    in 
    program p 0 s


let day_17 (data: string list) =
    let s, p = parse data in 
    let l = program_full p s in 
    l |> List.map string_of_int |> List.fold_left (fun a b -> a^","^b ) ""



let tdata = String.split_on_char '\n' "Register A: 117440
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"

let data = String.split_on_char '\n' "Register A: 5349
Register B: 0
Register C: 0

Program: 2,4,1,3,7,5,4,0,1,3,0,3,5,5,3,0"

let main () =
    let sol = if test then "" else day_17 data in

    if sol > "" then begin
        print_string sol; print_newline ()
    end	else if sol = "" then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()