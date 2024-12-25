open Advent_of_code;;
let test = false
let data = get_input 2024 21

 let pos_of_digit c = match c with 
    | '7' -> (0, 0)
    | '8' -> (0, 1)
    | '9' -> (0, 2)
    | '4' -> (1, 0)
    | '5' -> (1, 1)
    | '6' -> (1, 2)
    | '1' -> (2, 0)
    | '2' -> (2, 1)
    | '3' -> (2, 2)
    | '0' -> (3, 1)
    | 'A' -> (3, 2)
    | _ -> failwith "no digit"

 let digit_of_pos p = match p with 
    | (0, 0) -> '7'
    | (0, 1) -> '8'
    | (0, 2) -> '9'
    | (1, 0) -> '4'
    | (1, 1) -> '5'
    | (1, 2) -> '6'
    | (2, 0) -> '1'
    | (2, 1) -> '2'
    | (2, 2) -> '3'
    | (3, 1) -> '0'
    | (3, 2) -> 'A'
    | _ -> failwith "no digit here"


let pos_of_arrow c = match c with 
    | '^' -> 0, 1
    | 'A' -> 0, 2
    | '<' -> 1, 0
    | 'v' -> 1, 1
    | '>' -> 1, 2
    | _ -> failwith "no arrow"

let arrow_of_pos p = match p with 
    | 0, 1 -> '^'
    | 0, 2 -> 'A'
    | 1, 0 -> '<'
    | 1, 1 -> 'v'
    | 1, 2 -> '>'
    | _ -> print_int (fst p); print_int (snd p); failwith "no arrow here"


let dir_of_arrow c = match c with 
    | '^' -> -1, 0
    | '<' -> 0, -1
    | '>' -> 0, 1
    | 'v' -> 1, 0
    | _ -> failwith "no arrow"


let arrow_of_dir (di, dj) = 
    if di < 0 then Some '^'
    else if di > 0 then Some 'v'
    else if dj < 0 then Some '<'
    else if dj > 0 then Some '>'
    else None


(* let press_A (a, b, c) = 
    let (ia, ja) = pos_of_digit a in 
    let (ib, jb) = pos_of_arrow b in 
    let (ic, jc) = pos_of_arrow c in 
     match (a, b, c) with 
    |(_, 'A', 'A') -> None
    |(_, _, 'A') -> 
        let (di, dj) = dir_of_arrow b in
        if ia+di < 0 || ia+di > 3 || ja+dj < 0 || ja+dj > 2 || (ia+di, ja+dj) = (3,0)
        then None else Some (digit_of_pos (ia+di, ja+dj), b, c)
    |(_, _, _) ->
            let (di, dj) = dir_of_arrow c in
        if ib+di < 0 || ib+di > 1 || jb+dj < 0 || jb+dj > 2 || (ib+di, jb+dj) = (0,0)
        then None else Some (a, arrow_of_pos (ib+di, jb+dj), c)


let press_key (a, b, c) d =
    let (ic, jc) = pos_of_arrow c in 
    let di, dj = dir_of_arrow d in 
    if ic+di < 0 || ic+di > 1 || jc+dj < 0 || jc+dj > 2 || (ic+di, jc+dj) = (0,0)
        then None else Some (a, b, arrow_of_pos (ic+di, jc+dj))

let nbrs s = 
    List.filter_map Fun.id (press_A s :: List.map (press_key s) ['>';'<';'^';'v'])

let goto (a: char) (b: char) =
    let s0 = (a, 'A', 'A') in  
    let s1 = (b, 'A', 'A') in  
    let open Hashtbl in 
    let dist = create 1000 in 
    add dist s0 0;
    let do_nbr u v = 
        match find_opt dist v with 
        | Some _ -> None
        | None -> (add dist v (find dist u + 1); Some v)
    in
    let do_nbrs u =
        let l = nbrs u in 
        List.filter_map (do_nbr u) l
    in
    let rec bfs l = match l with 
        | [] -> ()
        | _ -> l |> List.map do_nbrs |> List.concat |> bfs

    in bfs [s0]; find dist s1


let rec do_digit_seq (l: char list) = match l with 
    | [] | [_] -> 0
    | x::y::q -> 
        goto x y + do_digit_seq (y::q) + 1

let score (s: string) = 
    let mult = int_of_string (String.sub s 0 (String.length s -1)) in 
     s |> String.to_seq |> List.of_seq |> List.cons 'A' |> do_digit_seq |> ( * ) mult

 *)
let day_21 (data: string list) =
    data |> List.map score |> List.fold_left (+) 0 


(*
let rec repeat k c = match k with 
    | 0 -> [] 
    | _ -> c :: repeat (k-1) c

let move_digit_robot (i0, j0) (i1, j1) : char list =
    let di, dj = i1-i0, j1-j0 in 
    let lr = if dj > 0 then repeat dj '>' else repeat (-dj) '<' in
    let ud = if di > 0 then repeat di 'v' else repeat (-di) '^' in
    if i0 = 3 && j1 = 0 then
        ud @ lr
    else
        lr @ ud
    
let move_arrow_robot (i0, j0) (i1, j1) : char list =
    let di, dj = i1-i0, j1-j0 in 
    let lr = if dj > 0 then repeat dj '>' else repeat (-dj) '<' in
    let ud = if di > 0 then repeat di 'v' else repeat (-di) '^' in
    if  i0 = 0 && j1 = 0 then
        ud @ lr
    else if dj < 0 || di > 0 then
        lr @ ud
    else ud @ lr

let rec do_digit_seq (l: char list) = match l with 
    | [] | [_] -> []
    | x::y::q -> 
        let (i0, j0) = pos_of_digit x in 
        let (i1, j1) = pos_of_digit y in 
        move_digit_robot (i0, j0) (i1, j1) @ 'A' :: do_digit_seq (y::q)

let rec do_arrow_seq (l: char list) = match l with 
    | [] | [_] -> []
    | x::y::q -> 
        let (i0, j0) = pos_of_arrow x in 
        let (i1, j1) = pos_of_arrow y in 
        move_arrow_robot (i0, j0) (i1, j1) @ 'A' :: do_arrow_seq (y::q)


let all_moves (s: string) =
    let r1 = s |> String.to_seq |> List.of_seq in
    let r2 = ('A' :: r1)  |> do_digit_seq in 
    let r3 = ('A' :: r2)  |> do_arrow_seq  in 
    let r4 = ('A' :: r3)  |> do_arrow_seq  in 
    (r1, r2, r3, r4)

let mega_move (s: string) =
    s |> String.to_seq |> List.of_seq |> List.cons 'A'
      |> do_digit_seq |> List.cons 'A'
      |> do_arrow_seq |> List.cons 'A'
      |> do_arrow_seq |> List.length

let day_21 (data: string list) =
    data |> List.map (fun s -> (mega_move s, String.sub s 0 (String.length s -1)))
         |> List.map (fun (x, t) -> print_int x; print_string (" " ^ t ^"\n");x * int_of_string t) |> List.fold_left (+) 0
 *)



let tdata = String.split_on_char '\n'
"029A
980A
179A
456A
379A"

let n_robs = 500

let solve s =
    let mult = int_of_string (String.sub s 0 (String.length s -1)) in
    let open Hashtbl in 
    let t = create 2000 in
    let a = pos_of_arrow 'A' in

    let rec mem n (i0, j0) (i1, j1) =
    (* button presses to get from i0,j0 to i1,j1 on robot (n-1),
       with robots 0 ... n-2 on button A before and after *)
        if n = 0 then abs(i0-i1) + abs(j0-j1) else
        match find_opt t (n, (i0, j0) , (i1, j1)) with 
        | Some r -> r 
        | None -> let res = 
            let ud, lr = arrow_of_dir ((i1-i0), 0), arrow_of_dir (0, (j1-j0)) in
            match ud, lr with 
            | None, None -> 0
            | None, Some dir 
            | Some dir, None -> 
                let dir = pos_of_arrow dir in 
                 mem (n-1) a dir + abs(j1-j0) + abs(i1-i0) + mem (n-1) dir a
            | Some ud, Some lr ->
            let ud, lr = pos_of_arrow ud, pos_of_arrow lr in 
            let ud_lr = mem (n-1) a ud + abs(i1-i0) + mem (n-1) ud lr + abs(j1-j0) + mem (n-1) lr a in
            let lr_ud = mem (n-1) a lr + abs(i1-i0) + mem (n-1) lr ud + abs(j1-j0) + mem (n-1) ud a in
            if (n = n_robs && i0 = 3 && j1 = 0) || (n < n_robs && i0 = 0 && j1 = 0) then
                ud_lr
            else if (n = n_robs && j0 = 0 && i1 = 3) || (n < n_robs && i1 = 0 && j0 = 0) then
                lr_ud
            else 
                min ud_lr lr_ud
        in add t (n, (i0, j0) , (i1, j1)) res; res
    in
    let rec do_digit_seq (l: char list) = match l with 
    | [] | [_] -> 0
    | x::y::q -> 
        let (i0, j0) = pos_of_digit x in 
        let (i1, j1) = pos_of_digit y in 
        mem n_robs (i0, j0) (i1, j1) + do_digit_seq (y::q) + 1
    in
    let res = s |> String.to_seq |> List.of_seq |> List.cons 'A'
      |> do_digit_seq in
    print_string (s^": "); print_int res; print_newline(); res*mult


let day_21 (data: string list) =
    data |> List.map solve |> List.fold_left (+) 0 


let main () =
    let sol = if test then -1 else day_21 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()

(* 
A:
digit: 
'^'; '^'; '<'; '<'; 'A'

arrow 1:
'<'; 'A'; 'A'; 'v'; '<'; 'A'; 'A'; '>'; '>'; '^'; 'A';

arrow 2:
'v'; '<'; '<'; 'A'; '>'; '>'; '^'; 'A'; 'A'; 'v'; '<'; 
'A'; '<'; 'A'; '>'; '>'; '^'; 'A'; 'A'; 'v'; 'A'; 'A'; 
'^'; '<'; 'A'; '>'; 'A'

B:
digit:
'<'; '<'; '^'; '^'; 'A'

arrow 1:
'<'; '<'; 'v'; 'A'; 'A'; '^'; '>'; 'A'; 'A'; '>'; 'A';

arrow 2:
'<'; '<'; 'v'; 'A'; 'A'; '>'; 'A'; '^'; '>'; 'A'; 'A'; 
'<'; 'A'; 'v'; '>'; 'A'; '^'; 'A'; 'A'; 'v'; 'A'; '^'; 
'A'; *)

(* 157892 *)