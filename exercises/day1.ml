(*_ means we don't care if we give it a name or not*)
let _ = print_endline "Hello world!"

(* [float_of_int] is used to do int -> float*)
let two_pi = 3.13 *. (float_of_int 2)
let ascii_to_int ch = int_of_char ch

(* ^ is the concatenation operator. *)
let str_concat_example = "abc" ^ "def"
let int_to_str n = string_of_int n
let char_to_str ch = String.make 1 ch
let string_to_char str idx = str.[idx]

(* Else is always mandatory *)
let always_false =
    if 2 > 3 then 5
    else 0

let increment x = x + 1
let result =
    let x = 42 in x + 1
let result' =
    let x = 42 in increment x

(* Exercise 1: *)
let abs_int n =
    if n < 0 then n * -1
    else n

let is_postv_no_match n =
    if n > 0 then "positive"
    else if n < 0 then "negative"
    else "zero"

(* It's possible to apply match instead of if-else.
    To still have conditions, use "when" and then
    the condition instead of a full value checking.*)
let is_postv_match n =
    match n with
    | _ when n > 0 -> "positive"
    | _ when n < 0 -> "negative"
    | _ -> "zero"

let sum_sequence n =
    let rec sum_sequence_rec n sum =
        if n = 0 then sum
        else sum_sequence_rec (n - 1) (sum + n)
    in sum_sequence_rec n 0

let fun_square_pow x = x * x
let fun_cubic_pow (x: int): int = x * x
let triple_mult a b c = a * b * c

let rec fact_not_tail n =
	if n <= 0 then 1
	else n * fact_not_tail (n - 1)

let rec fact_tail n acc =
    if n = 0 then acc
    else fact_tail (n - 1) (acc * n)

let a =
    let x = 1 in x * x

let abc_uppercase c =
    match c with
    | 'a' -> 'A'
    | 'b' -> 'B'
    | 'c' -> 'C'
    |  _  -> ' ' (* Must be exhaustive and must return char. *)

(* With conditions. *)
let compare_ints a b =
    match a with
    | _ when a > b -> 1
    | _ when a < b -> -1
    | _ -> 0

(* let _ =
    let result = string_of_int (fun_square_pow 4)
    in print_endline result *)

(* Executable main function.*)
(* let _ = print_endline (is_postv (-5)) *)