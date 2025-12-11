
open Advent_of_code;;
let test = false
let data = get_input 2025 11

let tdata = String.split_on_char '\n'
"aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out"

let tdata = String.split_on_char '\n'
"svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out"

let parse_line (s: string) : string * string list =
    let u::[succs] = 
        String.split_on_char ':' s
    in
    let succs =
        succs |> String.trim |> String.split_on_char ' ' |> List.map String.trim
    in
    (u, succs) 


let count_paths (adjs: int list array) (s: int) (t: int) : int =
    let n = Array.length adjs in
    let paths = Array.make n (-1) in (* paths.(u) = # of paths from u to t*)
    Printf.printf "%d %d" t n ; print_newline();
    paths.(t) <- 1;
    let rec mem_path u = 
        Printf.printf "%d" u; print_newline();
        if paths.(u) < 0 then begin
            paths.(u) <- List.fold_left (+) 0 (List.map mem_path adjs.(u))
        end;
        paths.(u);
    in
    mem_path s

(* PART 2 *)
let count_paths (adjs: int list array) (s: int) (t: int) (required: int list) : int =
    let n = Array.length adjs in
    let paths = Array.make_matrix n (List.length required + 1) (-1) in (* paths.(u).(k) = # of paths from u to t that visit k required vertices*)
    Printf.printf "%d %d" t n ; print_newline();
    paths.(t).(0) <- 1;
    let rec mem_path u k = 
        Printf.printf "%d" u; print_newline();
        if paths.(u).(k) < 0 then begin
            if List.mem u required then
                paths.(u).(k) <- List.fold_left (+) 0 (List.map (Fun.flip mem_path (k-1)) adjs.(u))
            else
                paths.(u).(k) <- List.fold_left (+) 0 (List.map (Fun.flip mem_path (k)) adjs.(u))
        end;
        paths.(u).(k);
    in
    mem_path s (List.length required)


let day_11 (data: string list) =
    let adjs =
        data |> List.map parse_line
    in
    let adjs = List.rev(("out",[])::(List.rev adjs)) in

    let nums = Hashtbl.create (List.length data + 1) in
    let target = (List.length data) in
    List.iteri (fun i (x, _) -> Hashtbl.add nums x i) adjs;
    let adjs = adjs 
        |> List.map (fun (x, succs) -> List.map (Hashtbl.find nums) succs)
        |> Array.of_list
    in 

    let source = Hashtbl.find nums "svr" in
    let required = List.map (Hashtbl.find nums) ["fft"; "dac"] in
    count_paths adjs source target required


let main () =
    let sol = if test then -1 else day_11 data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...
"
    else
        print_endline "Test"

let _ = main ()