open Unix

let get_input year day = 
	let fn = string_of_int year ^ "/input/day_" ^ string_of_int day ^".txt" in 
	if not (Sys.file_exists fn) then begin
		let _ = 
			Sys.command ("python3 download.py " ^ string_of_int year ^ " " ^ string_of_int day)
		in
		sleep 2
	end;
	let f = open_in fn in 

	let rec lines f = 
		try 
			let l = input_line f in
			l :: lines f 
		with 
			| End_of_file -> []
	in 
	lines f
;;


let write_solution (sol: int) = 
	let f = open_out "bla.txt" in begin
		output_string f (string_of_int sol);
		close_out f 
	end
;;

let write_solution_string (sol: string) = 
	let f = open_out "bla.txt" in begin
		output_string f sol;
		close_out f 
	end
;;