
open Advent_of_code;;
let test = false
let data = get_input 2024 22




let rec add_list x l =
    match l with 
        | [] -> []
        | y :: q -> y :: (y+x) :: add_list x q

let indices i = 
    [i] 
    |> add_list (6) |> List.filter (fun x -> 0 <= x && x < 24)
    |> add_list (-5) |> List.filter (fun x -> 0 <= x && x < 24)
    |> add_list (11) |> List.filter (fun x -> 0 <= x && x < 24)

let secret_matrix () =
    let m = Array.make_matrix 24 24 0 in
    let set i j = if  0 <= j && j <= 23 then m.(i).(j) <- 1-m.(i).(j) in
    for j = 0 to 23 do
        List.iter (fun i -> set i j) (indices j)
    done;
    m

let mult m1 m2 =
    let a = Array.length m1 in
    let b = Array.length m1.(0) in
    let c = Array.length m2.(0) in

    let m = Array.make_matrix a c 0 in
    for i = 0 to a-1 do
        for j = 0 to c-1 do
            for k = 0 to b-1 do
                m.(i).(j) <- m.(i).(j) lxor (m1.(i).(k) land m2.(k).(j))
            done
        done
    done;
    m


let truemult m1 m2 =
    let a = Array.length m1 in
    let b = Array.length m1.(0) in
    let c = Array.length m2.(0) in

    let m = Array.make_matrix a c 0 in
    for i = 0 to a-1 do
        for j = 0 to c-1 do
            for k = 0 to b-1 do
                m.(i).(j) <- m.(i).(j) + (m1.(i).(k) * m2.(k).(j))
            done
        done
    done;
    m
    

let vectmult m t =
    let a = Array.length m in
    let t' = Array.make 24 0 in
    for i = 0 to a-1 do
        for j = 0 to 23 do
            t'.(i) <- t'.(i) lxor (m.(i).(j) land t.(j))
        done
    done;
    t'
    
let truevectmult m t =
    let a = Array.length m in
    let t' = Array.make 24 0 in
    for i = 0 to a-1 do
        for j = 0 to 23 do
            t'.(i) <- t'.(i) + (m.(i).(j) * t.(j))
        done
    done;
    t'


let id n =
    let m = Array.make_matrix n n 0 in
    for i = 0 to n-1 do
        m.(i).(i) <- 1
    done;
    m
    

let rec pow m n = 
    if n = 0 then id 24 else
    let res = pow (mult m m) (n/2) in
    if n mod 2 = 1 then
        mult m res
    else
        res

let binvect n =
    let t = Array.make 24 0 in
    let x = ref n in
    for i = 0 to 23 do
        if !x mod 2 = 1 then t.(i) <- 1;
        x := !x / 2
    done;
    t

let num t =
    let res = ref 0 in 
    for i = 0 to 23 do
        res := !res * 2 + t.(23-i)
    done;
    !res
    
    

let nth_secret n secret =
    let m = secret_matrix() in 
    let m = pow m n in
    let t = binvect secret in 
    let t' = vectmult m t in 
    num t'



let digit_vect () =
    let t = Array.make_matrix 1 24 1 in
    for i = 1 to 23 do
        t.(0).(i) <- (2 * t.(0).(i-1)) mod 10
    done;
    t
    


let record_changes mats n =
    let v = binvect n in 
    let open Hashtbl in 
    let t = create 2001 in 
    (* t: (int*int*int*int -> int) maps each sequence of four ints to the corresponding 
       price *)
    let prices = Array.map (fun m -> (truevectmult m v).(0) mod 10) mats in 
    for i = 4 to 2000 do
        let a,b,c,d, e = prices.(i-4), prices.(i-3), prices.(i-2), prices.(i-1), prices.(i) in
        match find_opt t (b-a, c-b, d-c, e-d) with 
        | Some r -> ()
        | _ -> add t (b-a, c-b, d-c, e-d) e
    done;
    t
    

let precalc () =
    let s = secret_matrix () in
    let d = digit_vect() in
    let mats = Array.make 2001 [||] in 
    let rec write_mat i m =
        if i <= 2000 then begin
            mats.(i) <- truemult d m;
            write_mat (i+1) (mult m s)
        end
    in
    write_mat 0 (id 24);
    mats

let day_22 (data: string list) =
    let data = data |> List.map int_of_string in
    let mats = precalc () in 
    print_string "Done precalc"; print_newline();    
    let prices = List.map (record_changes mats) data in 

    let keys h = Hashtbl.fold (fun k v l -> k::l) h [] in

    let all_vars = prices |> List.map keys |> List.concat |> List.sort_uniq compare in 

    let score variat = 
        prices |>  List.filter_map (Fun.flip Hashtbl.find_opt variat)
        |> List.fold_left (+) 0
    in

    all_vars |> List.map score |> List.fold_left max 0


let main () =
    let sol = if test then -1 else day_22 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()