
open Advent_of_code;;
let test = false
let data = get_input 2024 18

let dirs = [(0, 1); (0, -1); (1, 0); (-1, 0)]

let nbrs n m (i, j) = 
    dirs |> List.map (fun (di, dj) -> (i+di, j+dj)) 
    |> List.filter (fun (i, j) -> 0 <= i && i < n && 0<= j && j < m)

let parse s =
    match List.map int_of_string (String.split_on_char ',' s) with 
    | x::y::_ -> (x, y)
    | _ -> failwith "parse"

let build l =
    let g = Array.make_matrix 71 71 true in
    List.iter (fun (j, i) ->g.(i).(j) <- false) l; g


let bfs g (is, js) (ie, je) =
    let n, m = Array.length g, Array.length g.(0) in
    let dist = Array.make_matrix n m (-1) in 
    dist.(is).(js) <- 0;
    let to_visit = [(is, js)] in 

    let visit (i, j) = 
        let l = nbrs n m (i, j) |>
                List.filter (fun (ii, jj) -> g.(ii).(jj) && dist.(ii).(jj) = -1) in 
        List.iter (fun (ii, jj) -> dist.(ii).(jj) <- dist.(i).(j)+1 ) l; 
        l
    in 
    let rec bfs_level l = match l with
        | [] -> () 
        | _ -> bfs_level (List.concat (List.map visit l))
    in
    bfs_level to_visit; dist.(ie).(je)


let rec split_n n l = match l, n with 
     | _, 0 -> [],l 
     | x::q, _ -> let lg, ld = split_n (n-1) q in x::lg, ld
     | _ -> failwith "no"


let rec build_and_bfs g l =
    match l with 
    | [] -> failwith "no block"
    | (j, i)::q -> 
        begin 
            g.(i).(j) <- false;
            if bfs g (0, 0) (70, 70) < 0 then (j, i) else build_and_bfs g q
        end

let day_18 (data: string list) =
    let lg, ld = data |> List.map parse |> split_n 1024 in
    let g = lg |> build in 
    build_and_bfs g ld

