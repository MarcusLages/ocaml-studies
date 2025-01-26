(* #require "zarith.top" *)

let ( ^^ ) x y =
    let rec aux acc x y =
        if y <= 0 then acc
        else aux (acc * x) x (y - 1)
    in
    aux 1 x y

(* Uses [zarith.top] big integer library;
    Use [opam install zarith] *)
(* let factorial n = 
    let rec aux n acc =
        if Z.equal n Z.zero then acc
        else aux (Z.pred n) (Z.mul acc n)
    in
    aux (Z.of_int n) Z.one *)

let f x = if x then x else x
let g x y = if y then x else x
let h x y z = if x then y else z
let i x y z = if x then y else y

let (--) floor ceil =
    let rec aux acc floor ceil =
        if floor > ceil then acc
        else aux (ceil::acc) floor (ceil - 1)
    in
    aux [] floor ceil

(* List.init 10 Fun.id *)
(* List.init 10 (fun x -> x + 1) *)

let fib_match_tr n =
    match n with
    | 1 -> Some 1
    | 2 -> Some 1
    | _ when n <= 0 -> None
    | _ ->
        let rec aux x1 x2 n =
            match n with
            | _ when n > 0 -> aux x2 (x1 + x2) (n - 1)
            | _ -> x2
        in
        Some (aux 1 1 (n - 2))

(** [flattens lst] flattens a list of lists [lst] into a list
        of its elements, with no inner lists;
        Tail recursive
*)
let flatten lst =
    let rec aux acc lst =
        match lst with
        | [] -> List.rev acc
        | (h::t1)::t2 ->
            aux (h::acc) (t1::t2)
        | []::t ->
            aux acc t
    in
    aux [] lst