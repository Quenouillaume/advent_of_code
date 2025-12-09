
open Advent_of_code;;
let test = false
let data = get_input 2025 9

let tdata = String.split_on_char '\n'
"7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3"
let expected = 24

let prod l1 l2 =
    l1 |> List.map (fun x -> List.map (fun y -> (x, y)) l2) |> List.concat

let list_seqs l =
    let rec seqs_head l h =
    match l with 
    | [x] -> [(x, h)]
    | x :: y :: q -> (x, y) :: seqs_head (y :: q) h
    | [] -> []
    in seqs_head l (List.hd l)

let area (x1, y1) (x2, y2) =
    (max x1 x2 - min x1 x2 + 1) * (max y1 y2 - min y1 y2 + 1)

(* Part 2: To know if a rectangle is valid, check all pairs of successive points
   in the data set and analyze their positions relative to the rectangle.
   O(n^3) D: *)
type position = 
    | Top | Bot | Left | Right  (* borders *)
    | Corner | Inside
    | Above | Under | At_left | At_right 
    | Away

let is_conflict pos1 pos2 = 
    match pos1, pos2 with
    | Inside, _ | _, Inside -> true
    | Corner, _ | _, Corner -> false
    | Away, _ | _, Away -> false
    | a, b when a = b -> false
    | Top, Above | Above, Top
    | Bot, Under | Under, Bot
    | Right, At_right | At_right, Right
    | Left, At_left | At_left, Left
        -> false
    | _ -> true

(* Return true if point 3 is inside rectangle made of points 1 and 2
   but NOT on the border.*)
let intersect (x1, y1) (x2, y2) (x3, y3) : bool =
    min x1 x2 <= x3 && x3 <= max x1 x2 && min y1 y2 <= y3 && y3 <= max y1 y2
    &&
    (x3 <> min x1 x2 && x3 <> max x1 x2) && (y3 <> min y1 y2 && y3 <> max y1 y2)


let on_corner (x1, y1) (x2, y2) (x3, y3) : bool =
    (x3 = min x1 x2 || x3 = max x1 x2) && (y3 = min y1 y2 || y3 = max y1 y2)

(* Return border of point inside rectangle made of points 1 and 2 *)
let get_position (x1, y1) (x2, y2) (x3, y3) : position =
    let x1, x2 = min x1 x2, max x1 x2 in let y1, y2 = min y1 y2, max y1 y2 in
    if on_corner (x1, y1) (x2, y2) (x3, y3) then Corner else
    if x3 = x1 && y1 <= y3 && y3 <= y2 then Left else
    if x3 = x2 && y1 <= y3 && y3 <= y2 then Right else
    if y3 = y1 && x1 <= x3 && x3 <= x2 then Top else
    if y3 = y2 && x1 <= x3 && x3 <= x2 then Bot else
    if intersect (x1, y1) (x2, y2) (x3, y3) then Inside else
    if x3 < x1 && y1 < y3 && y3 < y2 then At_left else
    if x3 > x2 && y1 < y3 && y3 < y2 then At_right else
    if y3 < y1 && x1 < x3 && x3 < x2 then Above else
    if y3 > y2 && x1 < x3 && x3 < x2 then Under else
    Away



type pt = int * int
let rec find_rect (seqs: (pt*pt) list) (pairs: (int*pt*pt) list) : int =
    match pairs with 
    | (a, p1, p2) :: q -> 
    if
        List.exists (fun (p3, p4) ->
            is_conflict (get_position p1 p2 p3) (get_position p1 p2 p4)
        )
        seqs 
    then
        find_rect seqs q
    else
        a 


(* Part 2 faster: Quadtree ??? O(n^2 log n) on average *)
let leaf_size = 10
type qt =
    | Leaf of pt * pt * int * pt list (* bounding box, then nb of points in leaf, then points *)
    | Node of pt * pt * qt array array (* bounding box, then four children *)

(* Return true if point 3 is inside rectangle made of points 1 and 2.*)
let soft_intersect (x1, y1) (x2, y2) (x3, y3) : bool =
    min x1 x2 <= x3 && x3 <= max x1 x2 && min y1 y2 <= y3 && y3 <= max y1 y2


(* return 0-1 index of subtree where (x, y) belongs
   in QT of bounding box (x1, y1, x2, y2) *)
let child_pos (x1, y1) (x2, y2) (x, y): int* int =
    let xm = (x2+x1) / 2 in
    let ym = (y2+y1) / 2 in
    let i = if x < xm then 0 else 1 in
    let j = if y < ym then 0 else 1 in
    i, j

let subdiv (q: qt) = match q with
    | Leaf ((x1, y1), (x2, y2), _, _) ->
        let xm = (x2+x1) / 2 in
        let ym = (y2+y1) / 2 in
        let child i j =
            let xa = if i = 0 then x1 else xm in
            let xb = if i = 0 then xm-1 else x2 in
            let ya = if j = 0 then y1 else ym in
            let yb = if j = 0 then ym-1 else y2 in
            Leaf ((xa, ya), (xb, yb), 0, [])
        in 
        let children =
            Array.init 2 (fun i -> Array.init 2 (fun j -> child i j))
        in
        Node ((x1, y1), (x2, y2), children)
    | _ -> failwith "Cannot subdiv node"


let rec insert (q: qt) ((x, y) as p: pt) : qt =
    match q with
    | Leaf ((x1, y1), (x2, y2), n, l) when n < leaf_size 
        -> Leaf((x1, y1),(x2, y2), n+1, (x, y) :: l)    
    | Leaf ((x1, y1), (x2, y2), n, l) -> begin
        let i, j = child_pos (x1, y1) (x2, y2) p in
        match subdiv q with 
        | Node(_, _, children) as q' -> begin
            children.(i).(j) <- insert children.(i).(j) (x, y);
            List.iter 
                (fun ((x', y') as p') ->
                    let i', j' = child_pos (x1, y1) (x2, y2) p' in
                    children.(i').(j') <- insert children.(i').(j') (x', y');
                )
                l;
            q'
        end
        | _ -> failwith  "Should not happen: leaf after subdiv"
    end
    | Node((x1, y1), (x2, y2), children) ->
        let i, j = child_pos (x1, y1) (x2, y2) p in
        children.(i).(j) <- insert children.(i).(j) (x, y);
        q


(* return true if any point of q is strictly inside rectangle p1 p2 *)
let rec insides (q: qt) (p1: pt) (p2: pt) : bool =
    match q with
    | Leaf(((xa, ya) as a), ((xb, yb) as b), n, l) ->
        if not (soft_intersect p1 p2 a || soft_intersect p1 p2 b || soft_intersect a b p1 || soft_intersect a b p2)
        then false
        else List.exists (intersect p1 p2) l
    | Node(((xa, ya) as a), ((xb, yb) as b), children) ->
        if not (soft_intersect p1 p2 a || soft_intersect p1 p2 b || soft_intersect a b p1 || soft_intersect a b p2)
        then false
        else Array.exists (Array.exists (fun q' -> insides q' p1 p2)) children

let rec find_rect (seqs: (pt*pt) list) (pairs: (int*pt*pt) list) (qtree: qt) : int =
    match pairs with 
    | (a, p1, p2) :: q -> 
    let x1 = min (fst p1) (fst p2) in
    let x2 = max (fst p1) (fst p2) in
    let y1 = min (snd p1) (snd p2) in
    let y2 = max (snd p1) (snd p2) in
    if insides qtree (x1, y1) (x2, y2) then find_rect seqs q qtree else
    if
        List.exists (fun (p3, p4) ->
            is_conflict (get_position p1 p2 p3) (get_position p1 p2 p4)
        )
        seqs 
    then
        find_rect seqs q qtree
    else
        a 


let day_9 (data: string list) =
    let pts = 
        data |> List.map (String.split_on_char ',')
        |> List.map (List.map int_of_string) |> List.map (fun (x::y::_) -> (x, y))
    in let pairs = 
        prod pts pts |> List.rev_map (fun (p1, p2) -> (area p1 p2, p1, p2))
        |> List.sort compare 
        |> List.rev
    in 
    let xs = List.map fst pts in
    let ys = List.map snd pts in
    let minx = List.fold_left min (List.hd xs) xs in
    let maxx = List.fold_left max (List.hd xs) xs in
    let miny = List.fold_left min (List.hd ys) ys in
    let maxy = List.fold_left max (List.hd ys) ys in

    let qtree = List.fold_left insert (Leaf((minx, miny), (maxx, maxy), 0, [])) pts in
    let seqs = list_seqs pts in 
    find_rect seqs pairs qtree
    (* in pts, seqs, pairs *)
 



let main () =
    if day_9 tdata <> expected then print_string "Failed on test" else begin
    let sol = day_9 data in
    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"
    end

let _ = main ()