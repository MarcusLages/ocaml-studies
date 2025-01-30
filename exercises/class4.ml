(* Variant - Sum Type *)
(* None or Some of 'a *)
(* 'a = type variable *)
type 'a myoption = None | Some of 'a
(* myoption = type constructor. Makes a type *)
(* None = value/data constructor. Makes a value *)

(* Result type usually returned by the syscall *)
type ('a, 'b) result = Ok of 'a | Error of 'b

(* Enum variant *)
type direction = North | East | South | West

type suit = Club | Diamond | Heart | Spades
(* Not possible to add restrictions to Num*)
type rank = Num of int | Jack | Queen | King | Ace

(* Polymorphic comparison between the order of the enums.*)
let i = Club < Diamond (* true *)
let i2 = Num 2 < Jack;; (* false *)
let i3 = Num 2 < Num 3

(* Recursive definition. *)
(* *= used for tuple definition *)
type 'a mylist = Nil | Cons of 'a * 'a mylist

let l = Cons (1, Nil)
let l1 = Cons (1, Cons (2, Nil))

let rec length l =
    match l with
    | Nil -> 0
    | Cons (_, t) -> 1 + length t

(* Midterm *)
let rec map f l =
    match l with
    | Nil -> Nil
    | Cons (h, t) -> Cons (f h, map f t)

let mapped = map (fun x -> x * x) (Cons (1, Cons (3, Nil)))

(* Recursive definition of a binary tree *)
(* Data at the beginning so it's easier to check the data *)
type 'a btree = Leaf | Node of 'a * 'a btree * 'a btree
let rec size t =
    match t with
    | Leaf -> 0
    | Node (_, l, r) -> 1 + (size l) + (size r)

let rec height t =
    match t with
    | Leaf -> 0
    | Node (_, l, r) -> 1 + max (height l) (height r)

(* Doesn't work with an usual tree, but it does with a btree *)
let rec map f t =
    match t with
    | Leaf -> Leaf
    | Node (x, l, r) -> Node (f x, map f l, map f r)

let rec mirror = function
    | Leaf -> Leaf
    | Node (x, l, r) -> Node (x, r, l)

(* Binary search tree *)
type 'a bstree = Leaf | Node of 'a * 'a bstree * 'a bstree
let bstree_empty = Leaf
let bstree_is_empty t = t = Leaf

let rec bstree_size t =
    match t with
    | Leaf -> 0
    | Node (_, l, r) -> 1 + bstree_size l + bstree_size r

let rec bstree_insert x t =
    match t with
    | Leaf -> Node (x, Leaf, Leaf)
    | Node (v, l, r) when x < v ->
        Node (v, bstree_insert v l, r)
    | Node (v, l, r) when x > v ->
        Node (v, l, bstree_insert v r)
    | _ -> t

let bstree_of_list l =
    List.fold_left (Fun.flip bstree_insert) bstree_empty l

let rec bstree_search x t =
    match t with
    | Leaf -> false
    | Node (v, l, r) when x < v ->
        bstree_search x l
    | Node (v, l, r) when x > v ->
        bstree_search x r
    | _ -> true

let rec bstree_max t =
    match t with
    | Leaf -> failwith "bstree_max: empty tree"
    | Node (v, _, Leaf) -> v
    | Node (_, _, r) -> bstree_max r

let rec bstree_delete x t =
    match t with
    | Leaf -> Leaf
    | Node (v, l, r) when x < v ->
        Node (v, (bstree_delete x l), r)
    | Node (v, l, r) when x > v ->
        Node (v, l, (bstree_delete x r))
    | Node (_, l, Leaf) ->
        l
    | Node (_, Leaf, r) ->
        r
    | Node (_, l, r) ->
        let max = bstree_max l in
        Node (max, bstree_delete max l, r)

(* ------------------------------------------------- *)
type 'a bstree = L | N of 'a * 'a bstree * 'a bstree
let bstree_empty = L
let bstree_is_empty t = t = L

let rec bstree_insert cmp x t =
    match t with
    | L -> N (x, L, L)
    | N (v, l, r) when (cmp x v) < 0 ->
        N (v, bstree_insert cmp x l, r)
    | N (v, l, r) when (cmp x v) > 0 ->
        N (v, l, bstree_insert cmp x r)
    | _ -> t

(* Whaat? *)
let bstree_of_list cmp l =
    List.fold_left (Fun.flip (bstree_insert cmp)) bstree_empty l

let rec bstree_max t =
    match t with
    | L -> failwith "bstree_max: empty tree"
    | N (x, _, L) -> x
    | N (_, _, r) -> bstree_max r

let rec bstree_delete x t =
    match t with
    | L -> L
    | N (v, l, r) when x < v ->
        N (v, (bstree_delete x l), r)
    | N (v, l, r) when x > v ->
        N (v, l, (bstree_delete x r))
    | N (_, l, L) ->
        l
    | N (_, L, r) ->
        r
    | N (_, l, r) ->
        let max = bstree_max l in
        N (max, bstree_delete max l, r)

(* ---------------------------------------------- *)
let rank_compare r1 r2 =
    match r1, r2 with
    | Num x1, Num x2 -> Int.compare x1 x2
    | Num _, _ -> -1 (* -1 because number must be earlier*)
    | _, Num _ -> 1
    | _, _ -> Stdlib.compare r1 r2

let c1 = rank_compare (Num 5) Queen

type card = rank * suit

(* Auto unpacking *)
let card_compare (r1, s1) (r2, s2) =
    let c = rank_compare r1 r2 in
    if c = 0 then Stdlib.compare s1 s2
    else c