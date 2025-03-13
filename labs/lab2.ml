(*
    Student: Marcus Vinicius Santos Lages
    A01392327
    Set 3-V
*)

(* Question 1a*)
(** [map_fold_left transf lst] maps every element of a list [lst]
    to another element in the return list according to the function
    [transf];
    Uses List.fold_left
*)
let map_fold_left transf lst= 
    List.rev (List.fold_left (fun acc x-> (transf x)::acc) [] lst)

let mapi f lst =
    let res, _ = List.fold_left (
        fun (l, idx) x ->
            (f idx x)::l, (idx + 1)
    ) ([], 0) lst
    in
    List.rev res

(** [map transf lst] maps every element of a list [lst]
    to another element in the return list according to the function
    [transf];
    Uses List.fold_right
*)
let map transf lst= 
    (* List.fold_right (fun x acc-> (transf x)::acc) lst [] *)
    mapi (fun _ -> transf) lst

(**/**)
let test_map () =
    assert(map (fun x -> x + 1) [] = []);
    assert(map (fun x -> x + 1) [1;2;3;4;5] = [2;3;4;5;6]);
    assert(map float_of_int [1;2;3;4;5] = [1.;2.;3.;4.;5.]);
    assert(map (fun x -> "Student " ^ x) ["M";"B";"R";"F"] =
        ["Student M";"Student B";"Student R";"Student F"])
(**/**)
    
(* Question 1b*)
(** [dedup lst] returns a list that has all the consecutive duplicates
    from [lst] removed;
    Tail-recursive
*)
let dedup lst =
    let aux elem acc =
        match acc with
        | h::t when h = elem -> acc
        | _ -> elem::acc
    in
    List.fold_right aux lst []

(**/**)
let test_dedup () =
    assert (dedup [] = []);
    assert (dedup [1;1;2;3;4;4;4;5] = [1;2;3;4;5]);
    assert (dedup [1;1;2;2;3;3;4;4;5;5] = [1;2;3;4;5]);
    assert (dedup [1;1;1;1;1;1;1] = [1]);
    assert (dedup [1;2;3;4;5] = [1;2;3;4;5])
(**/**)

(* Question 2a *)
(** [filteri f lst] filters elements from a list [lst] according
    to the function [f] that uses [(index, value)] of the current
    element
*)
let filteri f lst = 
    let rec aux idx acc = function
    | [] -> List.rev acc
    | h::t when f idx h -> aux (idx + 1) (h::acc) t
    | h::t -> aux (idx + 1) acc t
    in
    aux 0 [] lst

let filteri_fold f lst =
    let aux acc elem =
        match acc with
        | (idx, acc_lst) when f idx elem ->
            (idx + 1), (elem::acc_lst)
        | (idx, acc_lst) -> (idx + 1), acc_lst 
    in
    let _, filt_lst = List.fold_left aux (0, []) lst
    in
    List.rev filt_lst

(* Recreating filter using filteri *)
let filter f lst = filteri (fun idx -> f) lst
    
(**/**)
let test_filteri () =
    assert(filteri (fun idx elem -> true) [1;2] = [1;2]);
    assert(filteri (fun idx elem -> false) [1;2] = []);
    assert(filteri (fun idx elem -> idx mod 2 = 0) [1;2;3;4;5] = [1;3;5]);
    assert(filteri (fun idx elem -> (idx + elem) mod 2 = 0) [1;2;3;4;5] = []);
    assert(filteri (fun idx elem -> (idx + elem) mod 2 = 0) [0;1;2;3;4;5] = [0;1;2;3;4;5])
(**/**)

(* Question 2b *)
(** [every n lst] filters elements from a list [lst] so
    only every [n]th value is on the list;
    Requires [n] > 0
*)
let every n lst =
    filteri (fun idx elem -> (idx + 1) mod n = 0) lst

(**/**)
let test_every () =
    assert(every 1 [] = []);
    assert(every 1 [1;2;3;4;5;6;7;8;9;10] = [1;2;3;4;5;6;7;8;9;10]);
    assert(every 2 [1;2;3;4;5;6;7;8;9;10] = [2;4;6;8;10]);
    assert(every 3 [1;2;3;4;5;6;7;8;9;10] = [3;6;9])
(**/**)

(* Question 3a *)
(** [fold_while f acc lst] keeps folding left every element
    while the result is of the [f] function is not [None];
    Short circuit version of [List.fold_left];
    Tail recursive
*)
let rec fold_while f acc lst =
    match lst with
    | [] -> acc
    | h::t ->
        match f acc h with
        | None -> acc
        | Some acc'-> fold_while f acc' t

(**/**)
let test_fold_while () =
    assert(fold_while (fun acc x -> None) 0 [1;2;3;4;5] = 0);
    assert(fold_while (fun acc x -> Some (acc + x)) 0 [1;2;3;4;5] = 15);
    assert(fold_while (fun acc x -> if x < 3 then Some (acc + x) else None) 0 [1;2;3;4;5] = 3);
    assert(fold_while (fun acc x -> Some (acc + x)) 5 [] = 5)
(**/**)

(* Question 4b *)
(** [sum_while_less_than limit lst] keeps adding every next element on
    the list while the sum is strictly less than [limit];
    Returns a tuple (count, sum) with [count] being the count of how
    many integers were summed and [sum] being the total sum
*)
let sum_while_less_than limit lst =
    let aux acc x =
        let idx, sum = acc
        in
        if sum + x < limit then
            Some (idx + 1, sum + x)
        else
            None
    in
    fold_while aux (0,0) lst

(**/**)
let test_sum_while_less_than () =
    assert(sum_while_less_than 1 [1;2;3;4;5] = (0, 0));
    assert(sum_while_less_than 0 [] = (0, 0));
    assert(sum_while_less_than 20 [6;5;5;3;4] = (4, 19));
    assert(sum_while_less_than 4 [1;2;3;4;5;6] = (2, 3))

let test_all () =
    test_map ();    
    test_dedup ();
    test_filteri ();
    test_every ();
    test_fold_while ();
    test_sum_while_less_than ()
(**/**)