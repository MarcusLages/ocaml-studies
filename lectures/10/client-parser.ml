#require Angstrom

type name = Name of string * string
type client =
  | Org of int * string
  | Person of int * name

let name first last = Name (first, last)
let org id name = Org (id, name)
let person id name = Person (id, name)

open Angstrom
let a_char =
  (* what?! *)
  (string "\\(" *> '(') <|>
  (string "\\)" *> ')') <|>
  (string "\\," *> ',') <|>
  (string "\\n" *> '\n') <|>
  satisfy (fun c -> c != '(' && c != ')' && c != ',' && c != '\n')

let a_string =
  many1 a_char >>| fun l -> l |> List.to_seq |> String.of_seq

let an_id =
  take_while1 @@
    function
    | '0'..'9' -> true
    | _ -> false
  >>| int_of_string

let ws1 =
  take_while1 @@
    function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false

let ws =
  take_while @@
    function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false

let a_name =
  name <$> string "name(" *> a_string <* char ',' <*> a s_string <* char ')'
let an_org =
  org <$> string "org" *> an_id <* char ',' <*> a_string <* char ')'

let a_person =
  person <$> string "person(" *> an_id <* char ',' <*> a_name <* char ')'

let a_client = an_org <|> a_person

let parse_file =
  let ic = open_in file in
  let content = really_input_string ic (in_channel_length ic) in
  parse_string  ~consume:All (many1 (a_client <* ws)) content