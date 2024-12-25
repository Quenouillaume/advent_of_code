
open Advent_of_code;;
let test = false

let data = String.split_on_char '\n' "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"
let data = get_input 2024 16

let data = Array.of_list data
let n = Array.length data 
let m = String.length data.(0)


let dir = [(1,0);(-1,0);(0,1);(0,-1)]

let rot1 (di, dj) = (-dj, di)
let rot3 (di, dj) = (dj, -di)
let inv (di, dj) = (-di, -dj)



 let get t u = match Hashtbl.find_opt t u with 
        | None -> Int.max_int
        | Some x -> x

let set t v x = Hashtbl.add t v x


type 'a pq = L | N of 'a pq * 'a * 'a pq


let rec extract_min (p: 'a pq) : 'a * 'a pq =
    match p with 
    | L -> failwith "empty Q"
    | N (L, x, r) -> x, r
    | N (l, x, r) -> let m, l' = extract_min l in m, N (l', x, r)

let rec add p d u =
    match p with 
    | L -> N(L, u, L)
    | N(l, v, r) -> if get d u < get d v then N(add l d u, v, r)
                    else N(l, v, add r d u)

let rec height p = match p with 
    | L -> -1
    | N(l, _, r) -> 1 + max (height l) (height r)

let rec size p = match p with 
    | L -> 0
    | N(l, _, r) -> 1 +  (size l) + (size r)

type vert = (int*int)*(int*int)

let edges g ((i, j), (di, dj)) =
    let res = 
    (((i, j), rot1 (di, dj)), 1000) ::
    (((i, j), rot3 (di, dj)), 1000) :: []
    in let i' = i+di in let j' = j+dj in
    if 0 <= i' && i' < n && 0 <= j' && j' < m && g.(i).[j] != '#' then 
        (((i', j'), (di, dj)), 1)::res
    else 
        res



let dijkstra g (is, js) (di, dj) =
    let d = Hashtbl.create (n+m) in
   

    let relax u v w =
        if get d u + w < get d v then begin 
            set d v (get d u + w); true 
        end else 
            false
    in 

    let rec add_nbrs u l =
        List.fold_left (fun to_add (v, k)  -> if relax u v k then v::to_add else to_add) [] l
    in

    let rec extract l = match l with 
        | u::[] -> u, []
        | u::q -> let v, q' = extract q in if get d u < get d v then u, q else v, u::q'
        | [] -> failwith "empty list" 
    in

    let rec print_list l = match l with 
        | []-> print_newline ()
        | ((i, j), (di, dj))::q -> begin 
            print_int i; print_string " ";
            print_int j; print_string " ";
            print_int di; print_string " ";
            print_int dj; print_string "; "
            end
    in

    let rec visit p = 
        print_int (height p); print_string " ";
        print_int (size p); print_newline();
        if p = L then () else (*print_list l; *)
        let u, p' = extract_min p in
        let nbrs = edges g u in 
        let to_add = add_nbrs u nbrs in 
        visit (List.fold_left (fun p u-> add p d u) p' to_add) 
    in 
    if (di, dj) = (0, 0) then begin 
        set d ((is, js), (0, 1)) 0;
        set d ((is, js), (0, -1)) 0;
        set d ((is, js), (1, 0)) 0;
        set d ((is, js), (-1, 0)) 0;
        visit (List.fold_left (fun p u-> add p d u) L [
            ((is, js), (0, 1));
            ((is, js), (0, -1));
            ((is, js), (1, 0));
            ((is, js), (-1, 0));
        ])
    end else begin
        set d ((is, js), (di, dj)) 0;
        visit (List.fold_left (fun p u-> add p d u) L [((is, js), (di, dj))])
    end;
    d


let day_16 (data: string array) =
    let is, js = ref 0, ref 0 in 
    let ie, je = ref 0, ref 0 in 
    for i = 0 to n-1 do
        for j = 0 to m-1 do
            if data.(i).[j] = 'E' then begin 
                ie := i;
                je := j
            end;
            if data.(i).[j] = 'S' then begin 
                is := i;
                js := j
            end    
        done
    done;
    let dist_from_s = dijkstra data (!is, !js) (0, 1) in 
    let dist_to_e0 = dijkstra data (!ie, !je) (0, 0) in 

    let min_dist = get dist_to_e0 ((!is, !js), (0, 1)) in 


    let res = ref 0 in
    for i = 0 to n-1 do
        for j = 0 to m-1 do
            if data.(i).[j] != '#' then
            let best = 
                List.fold_left 
                    min 
                    Int.max_int 
                    (List.map 
                        (fun d -> get dist_from_s ((i, j), d) + get dist_to_e0 ((i, j), inv d)) 
                        dir
                    ) 
            in
            if best = min_dist then begin
                incr res
            end
        done
    done;
    !res


let main () =
    let sol = if test then -1 else day_16 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()