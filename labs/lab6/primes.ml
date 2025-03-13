(* 
    Marcus Vinicius Santos Lages
    A01392327
*)


(** [infstream] is a sum type that contains an ['a] value
    and an infinite stream of ['a] values *)
type 'a infstream = Cons of 'a * (unit -> 'a infstream)

(** [filter] receives an ['a infstream] and produces a filtered
    ['a infstream] based on [f]
 *)
let rec filter f (Cons (h, t)) =
    if f h then Cons (h, fun () -> filter f (t ()))
    else filter f (t ())

(** [from] produces an [int infstream] that increases by 1 for
    each term, starting at [n]
*)
let rec from n = Cons (n, fun () -> from (n + 1))

(** [take] gets the first [n] elements of an [infstream] and
    transforms them into a list
*)

let rec take n (Cons (h, t)) =
    if n <= 0 then []
    else h::take (n - 1) (t ())

(** [primes] is an [infstream] of prime numbers
    starting from 2
*)
let primes =
    let rec aux (Cons (h, t)) =
        Cons (h, fun () -> 
            aux (filter (fun x -> x mod h != 0) (t ()))
        )
    in
    2 |> from |> aux