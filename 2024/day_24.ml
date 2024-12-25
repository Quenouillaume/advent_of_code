
open Advent_of_code;;
let test = false
let data = get_input 2024 24

type op = Xor | And | Or 

let op_of_str s = match s with 
    | "XOR" -> Xor
    | "AND" -> And
    | "OR" -> Or
    | _ -> failwith "not an op"

type node = Input of bool | Output of op * string * string * (bool option ref)

let do_op o x y =
    match o with 
    | Xor -> (x || y) && (not x || not y)
    | And -> x && y
    | Or -> x || y

type circuit = (string, node) Hashtbl.t


let rec split_data l = match l with 
    | "" :: q -> [], q
    | x :: q -> let l1, l2 = split_data q in (x::l1, l2)

let build_circuit data =
    let inputs, connections = split_data data in 
    let c = Hashtbl.create 100 in 
    List.iter 
        (fun s -> let x::b::_ = String.split_on_char ':' s in
            let b = (int_of_string (String.sub s 5 1)) in
            Hashtbl.add c x (Input (b=1)) ) 
        inputs;

    List.iter
        (fun s -> let x :: op_str :: y :: _ :: z::[] = String.split_on_char ' ' s in
            Hashtbl.add c z (Output (op_of_str op_str, x, y, ref None))
            ) connections;
    c


let compute c = 
    let rec compute_node z =
        match Hashtbl.find c z with 
            | Input b -> b
            | Output(o, x, y, b_ref) -> 
                begin match !b_ref with 
                | Some b -> b 
                | None -> 
                    let bx, by = compute_node x, compute_node y in
                    let b = do_op o bx by in b_ref := Some b; b
                end
    in 
    let t = Array.make 100 (-1) in 
    Hashtbl.fold (fun z _ _ ->
        let b = compute_node z in 
        (* print_string z; print_string (if b then " true\n" else "false\n"); *)
        if z.[0] = 'z' then
            t.(int_of_string (String.sub z 1 2)) <- if b then 1 else 0) c ();
    let res = ref 0 in
    for i = 0 to 99 do
        if t.(99-i) <> -1 then begin
            res := !res*2 + t.(99-i) 
        end        
    done;
    !res

let is_io z =
    if '0' <= z.[1] && z.[1] <= '9' && '0' <= z.[2] && z.[2] <= '9' then 
        Some (int_of_string (String.sub z 1 2))
    else 
        None 

let fault c =
    let open Hashtbl in
    let grandparents zij = 
        let Some k0 = is_io zij in
        let Output (_, x, y, _) = find c zij in
        let check_parent x =   
            let Output (_, xx, yy, _) = find c zij in
            let check_grandparent xx =
                match is_io xx with 
                | Some k -> if k <> k0 && k <> k0-1 then begin 
                    print_string (zij ^" " ^ x ^ " " ^ xx ^"\n")
                    end
                | None -> ()
            in check_grandparent xx; check_grandparent yy
        in check_parent x; check_parent y
    in for i = 0 to 45 do
        let zij = "z" ^ (if i < 10 then "0" else "") ^ string_of_int i in
        grandparents zij
    done
    
let make_io s i = 
      s ^ (if i < 10 then "0" else "") ^ string_of_int i 

let try_add x y =
    let c = build_circuit data in
    swap c "qff" "qnw";
    swap c "pbv" "z16";
    swap c "z23" "qqp";
    swap c "z36" "fbq";

    let rec inp s x i =
        if i = 45 then () else begin
            Hashtbl.add c (make_io s i) (Input (x mod 2 = 1));
            print_int (x mod 2);
            inp s (x/2) (i+1)
        end
    in 

    let print_node z = match Hashtbl.find c z with 
            | Input b -> print_int (if b then 1 else 0)
            | Output(o, x, y, b_ref) -> 
                begin match !b_ref with 
                | Some b -> print_int (if b then 1 else 0)
                | None -> print_string "?"
            end
    in

    print_string "x: "; inp "x" x 0; print_newline ();
    print_string "y: "; inp "y" y 0; print_newline ();
    Hashtbl.fold (fun z _ _ -> print_string (z^": "); print_node z; print_newline()) c ();


    let z = compute c in 



    print_string "z:"; inp "_" z 0; print_newline (); print_int z


type role =
    | In of int
    | Out of int
    | XI of int (* xor inputs k:  xk XOR yk *)
    | Carry of int
    | IGC of int (* Immediate Gen Carry k: xk AND yk *)
    | PC of int (* Previous Carry j: in carry bit comes from carry gen at j *)
    | Other
    | Error

let role_str r = match r with 
    | In k -> "input " ^ string_of_int k
    | Out k -> "output " ^ string_of_int k
    | Carry k -> "carry " ^ string_of_int k
    | IGC k -> "immediate carry " ^ string_of_int k
    | PC k -> "previous carry " ^ string_of_int k
    | XI k -> "xor inputs " ^ string_of_int k
    | Error -> "ERROR"
    | Other -> "no role"

let rec get_role c z = 
    match Hashtbl.find c z  with 
    | Input b -> In (Option.get (is_io z))
    | Output(o, x, y, b_ref) -> begin
        let role_x, role_y = get_role c x, get_role c y in
        match o, role_x, role_y with 
        | And, In k, In k' -> if k=k' then IGC k else Error
        | Xor, In k, In k' -> if k=k' then if k = 0 then Out 0 else XI k else Error 

        | Xor, XI k', IGC k
        | Xor, IGC k, XI k' when k'=1 && k=0 -> Out 1 

        | And, XI k', IGC k
        | And, IGC k, XI k' -> if k'=k+1 then PC k' else Error 

        | And, XI k', Carry k
        | And, Carry k, XI k' -> if k'=k+1 then PC k' else Error 


        | Or, PC k', IGC k
        | Or, IGC k, PC k' -> if k=k' then Carry k else Error

        | Xor, Carry k, XI k'
        | Xor, XI k', Carry k -> if k'=k+1 then Out k' else Error       
        | _ -> Other
    end


let show_roles c =
    let roles = Hashtbl.fold 
        (fun z _ l -> (z, get_role c z)::l) 
        c [] in

    let roles = List.sort (fun (_, r1) (_, r2) -> compare r1 r2) roles in 
        List.iter (fun (z, r)  -> print_string (z^": "); print_string (role_str r); print_newline ()) 
        roles



let c = build_circuit data

let _ = 
swap c "qff" "qnw";
swap c "pbv" "z16";
swap c "z23" "qqp";
swap c "z36" "fbq"


let swap c x y =
    let open Hashtbl in
    let rx, ry = find c x, find c y in 
    add c x ry, add c y rx

let day_24 (data: string list) =
    let c = build_circuit data in 
    swap c "qff" "qnw";
    swap c "pbv" "z16";
    swap c "z23" "qqp";
    swap c "z36" "fbq";
    show_roles c;
    0



let main () =
    let sol = if test then -1 else day_24 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()