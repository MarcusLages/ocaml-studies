(* How result type is defined 
type ('a, 'b) result = Ok of 'a | Error of 'b *)
#require "angstrom"
open Angstrom

(* Checks if starts with prefix *)
  let x = parse_string ~consume:Prefix (char '(') "abc"

(* Checks if  contains the character*)
let x' = parse_string ~consume:All (char '(') "abc"

(* Checks if matches either *)
let x'' = parse_string ~consume:Prefix (char '(' <|> char 'a') "abc"

(* Returns a list of many of the checked character, returns Ok with empty list if none *)
let x''' = parse_string ~consume:Prefix (many (char 'a')) "b"
(* Returns a list of many of the checked character, returns Error *)
let x'''' = parse_string ~consume:Prefix (many1 (char 'a')) "b"

