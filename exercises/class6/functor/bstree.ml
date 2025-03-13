(* Module type *)
module type OrderedType = sig
    type t
    val compare : t -> t -> int
end

module type S = sig
    type elem
    type t = L | N of elem * t * t
    val empty: t
    val is_empty: t -> bool
    val insert: elem -> t -> t
    val of_list: elem list -> t
    val delete: elem -> t -> t
end

(* Functor to make a module.
    Module Ord implements and satisfies OrderedType
*)
(* : S is not necessary if specified in the .mli *)
module Make(Ord: OrderedType) = struct
    (* Binary search tree *)
    type elem = Ord.t
    type t = L | N of elem * t * t
    let empty = L
    let is_empty t = t = L

    (* We don't need the compare function anymore.contents
        We can use the compare from the passed in module.
    *)
    (* let rec insert cmp x t = *)
    let rec insert x t =
        match t with
        | L -> N (x, L, L)
        (* | N (v, l, r) when (cmp x v) < 0 -> *)
        | N (v, l, r) when (Ord.compare x v) < 0 ->
            N (v, insert x l, r)
        | N (v, l, r) when (Ord.compare x v) > 0 ->
            N (v, l, insert x r)
        | _ -> t

    (* Whaat? *)
    let of_list l =
        List.fold_left (Fun.flip insert) empty l

    let rec max t =
        match t with
        | L -> failwith "max: empty tree"
        | N (x, _, L) -> x
        | N (_, _, r) -> max r

    let rec delete x t =
        match t with
        | L -> L
        | N (v, l, r) when x < v ->
            N (v, (delete x l), r)
        | N (v, l, r) when x > v ->
            N (v, l, (delete x r))
        | N (_, l, L) ->
            l
        | N (_, L, r) ->
            r
        | N (_, l, r) ->
            let max = max l in
            N (max, delete max l, r)
end

(* module IntTree = Bstree.make(Int) *)