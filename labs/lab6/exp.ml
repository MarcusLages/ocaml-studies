(* 
    Marcus Vinicius Santos Lages
    A01392327
*)

(** [lazystream] is a sum type that contains an ['a] value [Cons]
    and a lazily evaluated stream of ['a] values using [Lazy.t] 
*)
type 'a lazystream = Cons of 'a * 'a lazystream Lazy.t

(** [from] produces a [float lazystream] that increases by 1. for
    each term, starting at [n] 
*)
let rec from n = Cons (n, lazy (from (n +. 1.)))

(** [pow] computes [n^exp] ([n] raised to the power of [exp]);
    Tail recursive
*)
let pow n exp =
    let rec aux acc e =
        if e <= 0. then acc
        else aux (n *. acc) (e -. 1.)
    in
    aux 1. exp

(** [fact] computes [n!] (factorial of [n]) *)
let fact n =
    let rec aux acc x =
        if x <= 0. then acc
        else aux (x *. acc) (x -. 1.)
    in
    aux 1. n

(** [map] applies function [f] to each element of a [lazystream],
    producing a new transformed [lazystream] based on [f]
*)
let rec map f (Cons (h, t)) =
    Cons (f h, lazy (map f (Lazy.force t)))

(** [exp_terms] generates a [lazystream] of terms of infinite
    series expansion of [e^n] (exponential function), where
    each term is computed as [(n^x) / x!] 
*)
let exp_terms n =
    0. |> from |> map (fun x -> (Float.pow n x) /. (fact x))


(** [sum] sums the first [n] terms of a [float lazystream]
*)
let rec sum n (Cons (h, t)) =
    if n <= 0 then 0.
    else h +. sum (n - 1) (Lazy.force t)


(** [exp] computes an approximation of [e^n] by summing the
    first [num] terms of the power series expansion of [e^n]
    (exponential function), where each term is computed as
    [(n^x) / x!] 
*)
let exp num n =
    sum num (exp_terms n)