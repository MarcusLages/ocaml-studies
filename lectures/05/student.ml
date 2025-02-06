type student = {id: string; name: string; gpa: float}

(*
let s = {id = "A11111111"; name = "homer simpson"; gpa = 25.5}

let make_student id name gpa = {id = id; name = name; gpa = gpa}
*)

(* simpler version of make_student *)
let make_student id name gpa = {id; name; gpa}

(* 
let s = make_student "A12345678" "bart simpson" 88.8
let s2 = {s with s.gpa = 90.0}
*)

let name s = s.name

(* alternative versions of name *)
let name' {id; name; gpa} = name
let name'' {name} = name

(* problem with records: several record types can have the same field name *)
type instructor = {id: string; name: string; salary: float}

let id {id} = id  (* applies to instructors (the "latest" type), NOT students *) 
type mut_student = {id: string; name: string; mutable gpa: float}

(*
let s3 = {id = "A12345678"; name = "homer simpson"; gpa = 25.5}
s3.gpa <- 30.0  (* change gpa *)
*)
