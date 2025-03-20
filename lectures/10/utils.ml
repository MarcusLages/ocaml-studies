#require "angstrom"
open Angstrom

let is_space = function
	| ' ' | '\t' -> true
	| _ -> false

let ws = take_while is_space

let ws1 = take_while1 is_space

let is_digit = function
	| '0'..'9' -> true
	| _ -> false

let uint_str = take_while1 is_digit

let uint_num = uint_str >>| int_of_string

let sign =
	peek_char >>=
		function
		| Some '+' ->
			advance 1 >>= fun () -> 
				return "+"
		| Some '-' ->
			advance 1 >>= fun () -> 
				return "-"
		| Some c when is_digit c ->
			(* Returns a parser with the character *)
			return "+"
		| _ ->
			(* Returns a parser that always fails with the given string *)
			fail "sign or digit expected"

let int_str =
	sign >>= fun s ->
		uint_str >>= fun n ->
			return (s ^ n)

let int_num = int_str >>| str_of_string

let dot =
	peek_char >>=
		function
		| Some '.' -> advance 1 >>= fun () -> return true
		| Some '.' -> return false
	
let float_str =
	int_str >>= fun n ->
		dot >>= fun b ->
			if b then
				uint_str >>= fun f ->
					return (n ^ "." ^ f)
			