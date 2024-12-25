
open Advent_of_code;;
let test = false
let data = get_input 2024 23

let has_t u = (u.[0] = 't')

let build_mat l =
    let open Hashtbl in
    let g = create (26*26) in

    let add_edge u v = 
        begin match find_opt g u with 
        | None -> add g u (create (26*26))
        | _ -> ()
        end;
        add (find g u) v 1 
    in

    let bi_add_edge u v = add_edge u v; add_edge v u in
    List.iter (fun (u, v) -> bi_add_edge u v) l;
    g


let triangles g =
    let open Hashtbl in 
    let triangle_1 u =
        let l = fold (fun u _ q -> u::q) (find g u) [] in
        let triangle_2 v =
            if v <= u then 0 else
            let triangle_3 w = 
                v < w && (has_t u ||has_t v ||has_t w) && find_opt (find g v) w <> None 
                && (print_string (u ^" " ^ v ^" " ^ w ^"\n"); true)
             in
            l |> List.filter (triangle_3) |> List.length
        in
        l |> List.map triangle_2 |> List.fold_left (+) 0
    in
    let l = fold (fun u _ q -> u::q) g [] in
    l |> List.map triangle_1 |> List.fold_left (+) 0


(* Part 2 *)

let print_comp c = 
    List.iter (fun s -> print_string (s ^ ",")) c; print_newline ()

let clique g =
    let open Hashtbl in 
    let verts = fold (fun u _ q -> u::q) g [] in

    let can_add c u = 
        List.for_all (fun v ->  u < v && find_opt (find g u) v <> None) c
    in

    let nbrs = create (26*26) in 
    List.iter (fun u -> let n = fold (fun v _ x -> 1 + x) (find g u) 0 in add nbrs u n) verts;
    let n_verts = List.length verts in 


    let rec biggest c n hi lo_nbrs = 
        if n + lo_nbrs < hi then (n, c, hi) else
        verts 
        |> List.filter (can_add c)
        |> List.fold_left 
            (fun (n', c', hi') u -> 
            let (nn, cc, hihi) =
             biggest (u::c) (n+1) (max hi (n+1)) (min lo_nbrs (find nbrs u)) 
            in (max n' nn, (if n' > nn then c' else cc), max hi' hihi)
            ) (n, c, hi)

    in let n, c, hi = biggest [] 0 0 n_verts in print_comp c; n



let tdata = String.split_on_char '\n'
"kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn"

let day_23 (data: string list) =
    let g = data |> List.map (fun s -> String.split_on_char '-' s)
    |> List.map (fun (u::v::_) -> (u, v))
    |> build_mat 
    in clique g 


 let main () =
    let sol = if test then -1 else day_23 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main () 