(** [digits n] returns the list of the digits of [n]
*)
let digits n =
    let rec aux acc = function
    | 0 when acc = [] -> [0]
    | 0 -> acc
    | x -> aux ((Stdlib.abs (x mod 10))::acc) (Stdlib.abs (x / 10))
    in
    aux [] n

(**/**)
let test_digits () =
    assert (digits 0 = [0]);
    assert (digits 12345  = [1;2;3;4;5]);
    assert (digits (-123) = [1;2;3])
(**/**)

(* Question 1b *)