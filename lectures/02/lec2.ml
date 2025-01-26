(** Lecture 2 *)

(** [length l] returns the number of elements in the list [l]; not 
    tail-recursive *)
let rec length l =
  match l with
  | [] -> 0
  | _ :: xs -> 1 + length xs

(**/**)
let test_length () =
  assert (length [] = 0);
  assert (length [1] = 1);
  assert (length [3;2;7;6;8] = 5)
(**/**)

(** [length_tr l] returns the number of elements in the list [l]; 
    tail-recursive *)
let length_tr l =
  let rec aux acc l =
    match l with
    | [] -> acc
    | _ :: xs -> aux (acc + 1) xs
  in
  aux 0 l

(**/**)
let test_length_tr () =
  assert (length_tr [] = 0);
  assert (length_tr [1] = 1);
  assert (length_tr [3;2;7;6;8] = 5)
(**/**)

(* [reverse l] returns a list consisting of the elements of [l] reversed
 * not tail-recursive *)
let rec reverse l =
  match l with
  | [] -> []
  | x :: xs -> reverse xs @ [x]

(**/**)
let test_reverse () =
  assert (reverse [] = []);
  assert (reverse [1] = [1]);
  assert (reverse [1;2;3;4] = [4;3;2;1])
(**/**)

(* [reverse l] returns a list consisting of the elements of [l] reversed;
 * tail-recursive *)
let reverse_tr l =
  let rec aux acc l =
    match l with
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] l

(**/**)
let test_reverse_tr () =
  assert (reverse_tr [] = []);
  assert (reverse_tr [1] = [1]);
  assert (reverse_tr [1;2;3;4] = [4;3;2;1])
(**/**)

(**
 * [take n l] returns a list consisting of the first [n] elements of [l];
 * returns [l] if [n] is greater then the length of [l]; returns [] if
 * [n <= 0]; not tail-recursive
 *) 
let rec take n l =
  if n <= 0 then []
  else
    match l with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs

(**
 * [take n l] returns a list consisting of the first [n] elements of [l];
 * returns [l] if [n] is greater then the length of [l]; returns [] if
 * [n <= 0]; tail-recursive version
 *) 
let take_tr n l =
  let rec aux acc n l =
    if n <= 0 then List.rev acc
    else
      match l with
      | [] -> List.rev acc
      | x :: xs -> aux (x :: acc) (n - 1) xs
  in
  aux [] n l

(** [every_other l] returns a list consisting of every other element of
 *  [l] starting from the first.
 *)  
let rec every_other l =
  match l with
  | [] -> []
  | [x] -> [x]
  | x :: _ :: xs -> x :: every_other xs

let test_every_other () =
  assert (every_other [] = []);
  assert (every_other [1] = [1]);
  assert (every_other [1;2;3;4] = [1;3]);
  assert (every_other [1;2;3;4;5] = [1;3;5])

(* this shows using guards in patterns
let rec elem x = function
  | [] -> false
  | y :: ys when y = x -> true
  | _ :: ys -> elem x ys
*)

let rec elem x = function
  | [] -> false
  | y :: ys -> y = x || elem x ys

let rec find_opt x l =
  match l with
  | [] -> 
    None
  | (k, v) :: l' when k = x ->
    Some v
  | _ :: l' ->
    find_opt x l'

let rec find x l =
  match l with
  | [] -> Failure "find: not found"
  | (k, v) :: l' when k = x -> v
  | _ :: l' -> find x l'

