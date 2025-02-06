(** Run program with:
	[1.] ocamlbuild file.native
*)

(* let rec number acc =
	try
		let line = read_line () in
		Printf.printf "%05d %s\n" acc line;
		number (acc + 1)
	with
	| End_of_file -> ()

let () = number 1 *)

(* in/out_channel:  *)
(* Exception if file doesn't exist *)
(* let ic = open_in "ref.ml"
let line = input_line ic
let () = close_in ic *)

let rec number ic acc =
	try
		Printf.printf "%05d %s\n" acc (input_line ic);
		number ic (acc + 1)
	with
	| End_of_file -> close_in ic

let () = 
	if Array.length Sys.argv = 1 then
		Printf.fprintf stderr
			"Yo ma'dude, u moron.\nUsage: %s <file>\n"
			Sys.argv.(0)
	else
		try
			number (open_in Sys.argv.(1)) 1
		with
		| Sys_error msg -> Printf.fprintf stderr "%s\n" msg

