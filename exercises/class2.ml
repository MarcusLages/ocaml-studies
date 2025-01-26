let list1 = [1;2;3]
let list2 = 1::2::[] (* Same as 1::(2::[])*)
let tuple_list = [(1, "hello"); (2, "world"); (3, "goodbye")]

(* Pattern Matching *)
let list2 = 1::2::[] (* Same as 1::(2::[])*)
let x1::x2::[] = list2 (* Not exhaustive*)
let [x1; x2] = list2 (* This method only works if you have exactly 2 elements in the list.*)
  let x1::x2::_ = [1;2;3;4] (* Ignore the rest. *)
let _::_::l = [1;2;3;4] (* Ignore first 2 elements and get the rest AS A LIST *)

let tuple_list = [(1, "hello"); (2, "world"); (3, "goodbye")]
(* To get world: *)
let _::(_, world)::_ = tuple_list

(* Not tail recursive. *)
let rec not_tr_length lst =
    (* Pattern matching, empty list or not *)
    match lst with
    | [] -> 0
    | _ :: t -> 1 + not_tr_length t

(**/**) (* This stops documentation about this file. *)
let test_not_tr_length () =
    assert (not_tr_length [] = 0);
    assert (not_tr_length [1] = 1);
    assert (not_tr_length [3;2;7;8;6] = 5)
(**/**)

(* [length lst]  *)
let length lst =
    let rec tr_length acc lst =
        match lst with
        | [] -> acc
        | _ :: t -> tr_length (acc + 1) t
    in
    tr_length 0 lst
    
(**/**) (* This stops documentation about this file. *)
let test_length () =
    assert (length [] = 0);
    assert (length [1] = 1);
    assert (length [3;2;7;8;6] = 5)
(**/**)

(* Inefficient because it uses append (@) so it goes
   all the way to the end. O(n^2)*)
let rec reverse lst =
    match lst with
    | [] -> []
    | h::t -> reverse t @ [h]

(**/**)
let test_reverse () =
    assert (reverse [] = []);
    assert (reverse [1] = [1]);
    assert (reverse [1;2;3;4] = [4;3;2;1])
(**/**)
    
    (* More efficient. Uses cons ::*)
    let reverse_tr lst =
        let rec aux acc lst =
            match lst with
            | [] -> acc
            | h::t -> aux (h::acc) t
        in
        aux [] lst
(**/**)
let test_reverse_tr () =
    assert (reverse_tr [] = []);
    assert (reverse_tr [1] = [1]);
    assert (reverse_tr [1;2;3;4] = [4;3;2;1])
(**/**)

(**
  * [take n l] returns a list consisting of the first [n] elements of [l];
  * returns [l] if [n] is greater than the length of [l]
*)
let rec take n l =
    if n <= 0 then [] (* If first because because we are matching n, not l yet. *)
    else
        match l with
        | [] -> []
        | h::t -> h :: take (n - 1) t

let take_tr n l =
    let rec aux acc n l =
        if n <= 0 then List.rev acc (* Because since we keep prepending, we need to reverse it. *)
        else
            match l with
            | [] -> List.rev acc (* Because since we keep prepending, we need to reverse it. *)
            | h::t -> aux (h::acc) (n - 1) t
        in
        aux [] n l

(* Takes every other element *)
let rec take_evo l =
    match l with
    | [] -> []
    | [x] -> [x]
    | h::_::t -> h :: take_evo t

let test_take_evo () =
    assert (take_evo [] = []);
    assert (take_evo [1] = [1]);
    assert (take_evo [1;2;3;4] = [1;3]);
    assert (take_evo [1;2;3;4;5] = [1;3;5])

let rec elem x = function (* Function makes it so the last element is unnamed and pattern matched*)
    | [] -> false
    | h::t when h = x -> true (* Guard: pattern matching + condition*)
    | _::t -> elem x t

(* Similar to the one above, but using logical OR (||)*)
let rec elem x = function
    | [] -> false
    | h::t -> h = x || elem x t

(**
*)
(* OPTION TYPE: either None or Some*)
let rec find_opt key lst =
    match lst with
    | [] -> None
    | (k, value)::t_lst when k = key ->
        Some value
    | _::t_lst -> find_opt key t_lst 

(* RETURNS EXCEPTION: (type failure) failwith message*)
let rec find key lst =
    match lst with
    | [] -> failwith "find: not found"
    | (k, value)::t_lst when k = key -> value
    | _::t_lst -> find key t_lst 

let test_all () =
    test_length ();
    test_not_tr_length ();
    test_reverse ();
    test_reverse_tr ();
    test_take_evo ()

(* let fun n lst =
    match lst with
    | x1::x2::t when n > 0 -> fun x1::(n-1)
    | _ -> [] *)
