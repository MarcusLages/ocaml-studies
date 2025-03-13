(* Leftist tree:
    Tree rank: shortest distance from a leaf
    rank of a leaf is 0;

    Leftist tree:
        rank of left size >= rank of right size
        key(Parent) <= key (N) for every node N
*)
(* Rank of node = shortest distance to a leaf;
    Rank of leaf = 0;
    Note: rank of node = 1 + min (rank of left/right);
    Note: key = value
    2 properties:
        - Leftist: rank of left >= rank of right
        - Min heap: key of parent <= key of children node
*)
module Leftist = struct
    type 'a t = L | N of int * 'a * 'a t * 'a t
    exception Empty

    let rank = function
        | L -> 0
        | N (rk, _, _, _) -> rk

    let empty = L
    let is_empty t = t = L

    let rec merge t1 t2 =
        match t1, t2 with
        | L, t | t, L -> t
        | N (_, x1, _, _), N (_, x2, _, _) when x2 < x1 ->
            merge t2 t1
        | N (_, x, l, r), t ->
            let t' = merge t r in
            if rank t' <= rank l then
                N (1 + rank t', x, l, t')
            else
                N (1 + rank l, x, t', l)

    let insert x t =
        merge t (N (1, x, L, L))

    let of_list lst =
        List.fold_left (Fun.flip insert) empty lst

    let print pr t =
        let rec p level t =
            Printf.printf "%*s" (3 * level) "";
            match t with
            | L -> Printf.printf "-\n"
            | N (rk, x, l, r) ->
                pr x;
                Printf.printf "/%d\n" rk;
                p (level + 1) l;
                p (level + 1) r;
        in
        p 0 t

    let get_min t = function
        | L -> raise Empty
        | N (_, x, _, _) -> x

    let delete_min = function
        | L -> raise Empty
        | N (_, x, l, r) ->
            merge l r
end

module M = struct
    let f x = x * x
end

module type S = sig
    val f: int -> int
end

(* M' is M that satisfies S or of "type" S *)
module M' = (M : S)
(*  M' : S'
    M' can be further constrained by another module type *)

(* M2 has both g and f functions *)
module M2 = struct
    include M
    let g x = x * x * x
end

(* Purpose of include is to extend a module *)
module ListExt = struct
    include List
    let to_array lst =
        Array.of_list lst
end

(* Remember to use #load_rec/manually #load the dependency *)
module LeftistExt = struct
    include Leftist
    let sort l =
        let open Leftist in
        let rec aux t acc =
            if is_empty t then List.rev acc
            else aux (delete_min t) (get_min t :: acc)
        in
        aux (of_list l) []
end

(* Red-black tree:
    - regular binary tree, but solves the problem of unbalanced
    - No 2 consecutive nodes of red nodes
    - Every path from roof to leaf has the same number of black nodes
    - New nodes included are always red at first
*)