(* Question 1a *)
(** [digits n] returns the list of the digits of [n];
    Tail recursive
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
(** [int_of_digits lst] returns a number composed of all the numbers
    in the list [lst] as digits;
    Not tail-recursive
*)
let int_of_digits lst =
    let aux x acc =
        let value, base = acc
        in
        value + (x * base), base * 10
    in
    let result, _ = List.fold_right aux lst (0, 1)
    in
    result

(**/**)
let test_int_of_digits () =
    assert (int_of_digits [0] = 0);
    assert (int_of_digits [1;2;3;4;5] = 12345);
    assert (int_of_digits [1;0;0;0;0;3] = 100003)

let test_part1 () =
    test_digits ();
    test_int_of_digits ()
(**/**)