(**
    [factorial n] is [n!]
    Requires: [n >= 0]
*)
let rec factorial n = 
    if n = 0 then 1
    else n * factorial (n - 1)

let factorial_tr n =
    let rec aux n acc =
        if n = 0 then acc
        else aux (n - 1) (n * acc)
    in
    aux n 1

let rec gcd a b =
    if b = 0 then a
    else gcd b (a mod b)

let square_root a =
    let next x = 0.5 *. (x +. a /. x) in
    let good_enough x = abs_float (x *. x -. a) < 0.000001 in
    let rec aux x =
        if good_enough x then x
        else aux (next x)
    in aux 1.

let square_root' n a =
    let next x = 0.5 *. (x +. a /. x) in
    let rec aux i x =
        if i = 0 then x
        else aux (i - 1) (next x)
    in aux n 1.

(* Very inefficient fibonacci. *)
let rec fib n =
    if n = 1 then 0
    else if n = 2 then 1
    else fib (n - 1) + fib (n - 2)

(**
    [fib n] returns the n-th fibonacci number.
    Requires: [n >= 1]
*)
let rec fib_match n = 
    match n with
    | 1 -> 0
    | 2 -> 1
    | _ -> fib (n - 1) + fib (n - 2)
    (* | n -> fib (n - 1) + fib (n - 2) *)

let rec fib_match n = 
    let rec aux n a b =
        if n = 1 then a
        else aux (n - 1) b (a + b)
    in
    aux n 0 1
    (* | n -> fib (n - 1) + fib (n - 2) *)

let add a b = a + b
(* let y = add 1 -2 *) 
let y = add 1 (-2) (* Function calls have higher precedence than operators
                      so use parenthesis on negative functions*)

(* Creates precedence. *)
let ( @@ ) f x = f x
(* val (@@) : ('a -> 'b) -> 'a -> 'b = <fun> *)
let y2 = add 1 @@ -2

let ( |> ) x f = f x;
(* val (|>) : 'a -> ('a -> 'b) -> 'b = <fun> *)

(* let square x = x * x
let y5 = square (inc (square (inc 3)))
let y4 = 3 |> inc |> square |> inc |> square *)

(* let integral f a b n = *)

(* let die () =
    let rand = Random.int 5 in
    rand + 1 *)

(* Function square
    Function to_the_forth *)
(* let square x = x*x *)
let forth x = 
    let square x = x*x
    in (square x)*(square x)    
(* x*x*x*x = (x*x)*(x*X) = square(x)*square(x) *)