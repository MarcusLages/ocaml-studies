(** [digits n] returns the list of the digits of [n]
*)
let digits n =
    let rec aux acc = function
    | 0 when acc = [] -> [0]
    | 0 -> acc
    | x -> aux ((Stdlib.abs (x mod 10))::acc) (Stdlib.abs (x / 10))
    in
    aux [] n