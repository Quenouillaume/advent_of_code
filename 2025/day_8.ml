
open Advent_of_code;;
let test = false
let data = get_input 2025 8

let tdata = String.split_on_char '\n'
"162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689"
let expected = 40 


type pt = int * int * int

let sqeucl (x1, y1, z1) (x2, y2, z2) =
    let dx = (x2 - x1) in
    let dy = (y2 - y1) in
    let dz = (z2 - z1) in
    dx * dx +  dy * dy + dz * dz

(* union find with array *)
let rec find uf i =
        if uf.(i) = i then i else
        let j = find uf uf.(i) in
        uf.(i) <- j;
        j

    let rec union uf i j =
        let pi, pj = find uf i, find uf j in
        uf.(pi) <- pj


let connect (t: pt array)  : int  =
    let n = Array.length t in
    let uf = Array.init n (Fun.id) in

    let dist i j =
        sqeucl t.(i) t.(j)
    in

    let swap t i j =
        let tmp = t.(i) in
        t.(i) <- t.(j);
        t.(j) <- tmp
    in

    let find_closest () =
        let res = ref None in
        let shortest_d = ref None in
        for i = 0 to n-1 do
            for j = i+1 to n-1 do
                if find uf i <> find uf j && (!res = None || dist i j < Option.get !shortest_d) then begin
                    res := Some (i, j);
                    shortest_d := Some (dist i j)
                end
            done
        done;
        Option.get !res
    in        

    let connect_uf k =
        let res = ref ((0, 0, 0),(0, 0, 0)) in
        for i = 0 to k-1 do
            let (i, j) = find_closest () in
            if find uf i <> find uf j then begin
                res := (t.(i), t.(j));
                (* let xi, yi, zi = t.(i) in *)
                (* let xj, yj, zj = t.(j) in *)
                (* Printf.printf "Connecting %d (%d, %d, %d) and %d (%d, %d, %d)\n" i xi yi zi j xj yj zj; *)
                let pi, pj = find uf i, find uf j in
                (* Printf.printf "They were in circuits %d and %d\n" pi pj; *)
                union uf i j
            end
        done;
        !res
    in
    let ((x1, _, _), (x2, _, _)) = connect_uf (n-1) in
    x1*x2


(*  PART 1
    let sizes = Array.make n 0 in
    for i = 0 to n-1 do
        sizes.(find uf i) <- sizes.(find uf i) + 1
    done;

    (* Array.iteri (fun i x -> Printf.printf "Circuit no. %d of size %d\n" i x) sizes; *)

    let max_tab t n =
        let i0 = ref 0 in
        for i = 0 to n-1 do
            if t.(i) > t.(!i0) then i0 := i 
        done;
        !i0
    in

    let m1 = max_tab sizes (n-1) in
    swap sizes m1 (n-1);
    let m2 = max_tab sizes (n-2) in
    swap sizes m2 (n-2);
    let m3 = max_tab sizes (n-3) in
    swap sizes m3 (n-3);
    sizes.(n-1) * sizes.(n-2) * sizes.(n-3)

 *)


 (* PART 2 less bad: sort edges then iterate: O(n² log n) *)
let connect (t: pt array)  : int  =
    let n = Array.length t in
    let uf = Array.init n (Fun.id) in

    let dist i j =
        sqeucl t.(i) t.(j)
    in

    let swap t i j =
        let tmp = t.(i) in
        t.(i) <- t.(j);
        t.(j) <- tmp
    in

    let res = ref (0, 0) in

    let edges = List.concat (List.init n (fun i -> List.init n (fun j -> (dist i j, i, j)))) in
    let edges = List.sort compare edges in
    let rec make_connections l todo =
        match todo, l with
            | 0, _ | _, [] -> ()
            | _, (d, i, j) :: q -> 
            if find uf i <> find uf j then begin
                res := (i, j);
                union uf i j;
                make_connections q (todo-1)
            end else make_connections q todo
    in
    make_connections edges (n-1);
    let i, j = !res in
    let ((x1, _, _), (x2, _, _)) = t.(i), t.(j) in
    x1*x2


        



let day_8 (data: string list) =
    data |> List.map (String.split_on_char ',') |>
    List.map (List.map int_of_string)
    |> List.map (fun (x::y::z::_) -> (x, y, z))
    |> Array.of_list |> connect

let main () =
    let sol = if test then -1 else day_8 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()