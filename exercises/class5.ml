(* Product type
    Sum type: variant/ union/struct
    Tuple, objects, option are a product type *)
type student = {
    id: string;
    name: string;
    gpa: float
}

let s = {
    id = "A1111111";
    name = "homer simpson";
    gpa = 10.2
}


let make_student id name gpa = {
    id = id;
    name = name;
    gpa = gpa
}
    
let s2 = make_student "A66666666" "monty burns" 99.99

let make_student id name gpa = { id; name; gpa }

(* Ways to get the structure type *)
let name s = s.name
let name {id = i; name = n; gpa = g} = n
let name {id; name; gpa} = name
let name {name} = name

type instructor = {
    id: string;
    name: string;
    salary: float
}

let i = {id = "A3413412"; name = "bart simpson"; salary = 100.0}
(* name i;; (* Doesn't work because instructor was created after*)*)

type student' =
    Student of {id: string; name: string; gpa: float}
let s4 = Student {name = "ned flanders"; gpa = 88.8; id = "A00000"}

type student'' =
    Student of {id: string; name: string; cheat: int -> int}

(* mutable allows variables in a record *)
type student''' = {id: string; name: string; mutable gpa: float}

let s5: student''' = {
    id = "A1111111";
    name = "homer simpson";
    gpa = 10.2
}

(* Allowed in functions *)
let nothing () = s5.gpa<-90.0

(* Creates a new student with something changed *)
let z = {s5 with gpa = 100.0}

(* ------------------------------------------- *)
(* References *)
(* Like a pointer *)
let r = ref 5 (* ref = {contents = 5} *)

let deref_r = !r (* Dereference *)
(* let nothing () =
    r := 6  (* Assignment with dereference *)
    in
    decr r  (* Decrement with dereference *)
    in
    incr r  (* Increment with dereference *) *)

type 'a ref = {mutable contents: 'a}

(* Ref type implementation *)
let ref x = {contents = x}
let ( ! ) r = r.contents
let ( := ) r v = r.contents <- v
let incr r = r.contents <- r.contents + 1
let decr r = r.contents <- r.contents - 1

(* x and y share the same ref value. *)
let x = ref 6;;
let y = x;;

(* Sharing the record *)
let s6 = {id = "A234123"; name= "dsfasd"; gpa = 10.0}
let s6' = s6

(* Impure functions *)
(* ; is used for sequencing. Throws away the return value. If you want
    to throw away something that is not a unit, use [ignore f]
*)
let counter =
    let c = ref 0 in
    fun () -> (incr c; !c)

(* --------------------------------------------------------- *)
(* Array type *)
let a = [|1; 2; 3|]
let a1 = a.(1);;
let a1 = a.(3);; (*Out of bounds*)
let nothing () = a.(1) <- 0
let c = Array.copy a

let nothing () =
    for i = 0 to 2 do
        a.(i) <- a.(i) + 1 done

(* cmd args = Sys.argv *)
let prints () =
    let x = 10 in
    Printf.printf "%d\n" x;
    print_endline "";
    Printf.fprintf stderr "error"

let print_array arr =
    let len = Array.length arr in
    for i = 0 to len - 1 do
        Printf.printf "%d " arr.(i)
    done;
    print_newline ()

let echo () =
    let last = Array.length Sys.argv - 1 in
    for i = 1 to last do
        Printf.printf (if i = last then "%s\n" else "%s") Sys.argv.(i) (* Ternary operator *)
    done

let scan () =
    Scanf.scanf " %d" (fun x -> 2 * x) (* The space in front of the thing is important*)

let scan' () =
    Scanf.scanf " %s" (fun x -> x) (* Scans only one word *)

let scan_line () =
    read_line ()