let rec map f l =
  match l with
  | [] -> []
  | x :: xs -> f x :: map f xs

let rec map_tr f l =
  let rec aux acc l =
    match l with
    | [] -> List.rev acc
    | x :: xs -> aux (f x :: acc) xs
  in
  aux [] l

let rec filter f l =
  match l with
  | [] -> []
  | x :: xs -> 
    if f x then x :: filter f xs
    else filter f xs

let rec take_while f l =
  match l with
  | [] -> []
  | x :: xs ->
    if f x then x :: take_while f xs
    else []

(* assume l is in ascending order; insert x into l keeping the order *)
let rec insert1 x l =
  match l with
  | [] -> [x]
  | y :: ys ->
    if x <= y then x :: l
    else y :: insert1 x ys

let rec sort1 l =
  match l with
  | [] -> []
  | x :: xs -> insert1 x @@ sort1 xs

let rec insert2 cmp x l =
  match l with
  | [] -> [x]
  | y :: ys when cmp x y <= 0 -> x :: l
  | y :: ys -> y :: insert2 cmp x ys

let rec sort2 cmp l =
  match l with
  | [] -> []
  | x :: xs -> insert2 cmp x @@ sort2 cmp xs

let rec insert ~cmp x l =
  match l with
  | [] -> [x]
  | y :: ys ->
    if cmp x y <= 0 then x :: l
    else y :: insert ~cmp x ys

let rec sort ~cmp l =
  match l with
  | [] -> []
  | x :: xs -> insert ~cmp x @@ sort ~cmp xs

let rec fold_left f acc l =
  match l with
  | [] -> acc
  | x :: xs -> fold_left f (f acc x) xs

let rec fold_right f l acc =
  match l with
  | [] -> acc
  | x :: xs -> f x (fold_right f xs acc)

let rev l = fold_left (fun acc x -> x :: acc) [] l

(* List.fold_left, List.fold_right, Fun.flip *)
