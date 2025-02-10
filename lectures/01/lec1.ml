(** [factorial n] is [n!]
    Requires: [n >= 0]
    This function is not tail-recursive.
*)
let rec factorial n =
  if n = 0 then 1
  else n * factorial (n - 1)

(** [factorial_tr n] is [n!]
    Requires: [n >= 0]
    This function is tail-recursive.
*)
let factorial_tr n =
  let rec aux n acc =
    if n = 0 then acc
    else aux (n - 1) (n * acc)
  in
  aux n 1

(** [fib n] is the n-th fibonacci number
    Requires: [n >= 1]
    This version is not efficient
*)    
let rec fib n =
  match n with
  | 1 -> 0
  | 2 -> 1
  | _ -> fib (n - 1) + fib (n - 2)

(** [fib' n] is the n-th fibonacci number
    Requires: [n >= 1]
    Efficient version
*)    
let fib' n =
  let rec aux n a b =
    if n = 1 then a
    else aux (n - 1) b (a + b)
  in
  aux n 0 1

(** [square_root a] returns an approximate value of the square root of [a].
 *  The return value [x] satisfies [|x * x - a| < 0.000001].
 *  Requires: [a >= 0].
 *)  
let square_root a =
  let next x = 0.5 *. (x +. a /. x) in
  let good_enough x = abs_float (x *. x -. a) < 0.000001 in
  let rec aux x = if good_enough x then x else aux (next x) in
  aux 1.

(* *)
let square_root' n a =
  let next x = 0.5 *. (x +. a /. x) in
  let rec aux i x = if i = 0 then x else aux (i - 1) (next x) in
  aux n 1.
  
