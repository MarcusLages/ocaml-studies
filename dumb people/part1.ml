let x = 5 + 5 (* Expressio: 5 + 5, value: 10*)

let w =
    let y = 5
    in
    y * y

let f n =
    let aux acc n = n
    in
    aux 0 n

let w' =
    let a = 5
    in
    let b = 3
    in
    let r = 5
    in
    let dtyujdgfhjdfg a b =
        a * a * b * a * b * a * b * a
    in
    dtyujdgfhjdfg a b

(* let a .. in .... in .... (function or expression) *)



(* let average = *)
    (* DO NOT USE A FUNCTION YET, just define deliberate values for a and b *)
    (* define a and b *)
    (* define helper functions add and div *)
    (* calculate value *)


let add a b = a + b

let asdfa =
    let a = 32
    in
    let b = 1
    in
    add a b;;


(* let print s = print_endline s
print "dfafgdaf"
let s = "sdfgsdf"
in
s ^ " " *)

let f1 =
    let add a b = a + b 
    in
    let add' a b = a + a + b
    in
    let a = 4
    in
    let b = 5
    in
    (add a b) + (add' a b)

let w' =
    let a = 5
    in
    let b = 3
    in
    a * a * b

let average = 
    let add a b = a + b
    in
    let a = 2
    in
    let b = 2
    in
    let div a b = a / b
    in
    div (add a b) 2

(* average has:
    add a b,
    a = 2
    b = 1
    div a b
        *)

let f'' a b c = a - b + c

let x' =
    f'' (5 + 2) (5 - 2) (5 * 2)

let rec mult a b =
    if a = 0 || b = 0 then
        0
    else
        a + mult a (b - 1)

let f a =
    match a with
    (* | h1::t -> 1::[] *)
    | _ when a > 1 -> 1 
    | _ when a < 1 -> -1 
    | _ -> 0

let rec rev l =
    match l with
    | [] -> []
    | h::t ->
        [h] @ rev t

let rev' l =
    let rec aux acc l' =
        match l' with
        | [] -> acc
        | h::t ->
            aux (h::acc) t
    in
    aux [] l

let occ n lst =
    let rec aux lst' c =
    match lst' with
    | [] -> c
    | x::xs -> if x = n
        then aux xs (c + 1)
        else aux xs c
    in
    aux lst 0