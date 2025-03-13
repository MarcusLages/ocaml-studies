module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t = L | N of elt * t * t
  val empty : t
  val is_empty : t -> bool
  val insert : elt -> t -> t
  val of_list : elt list -> t
  val delete : elt -> t -> t
end

module Make(Ord : OrderedType) = struct
  type elt = Ord.t
  type t = L | N of elt * t * t

  let empty = L

  let is_empty t = t = L

  let rec insert x t =
    match t with
  | L -> N (x, L, L)
  | N (v, l, r) when Ord.compare x v < 0 ->
      N (v, insert x l, r)
  | N (v, l, r) when Ord.compare x v > 0 ->
      N (v, l, insert x r)
  | _ -> t

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
  | N (v, l, r) when Ord.compare x v < 0 ->
      N (v, delete x l, r)
  | N (v, l, r) when Ord.compare x v > 0 ->
      N (v, l, delete x r)
  | N (_, l, L) -> l
  | N (_, L, r) -> r
  | N (_, l, r) ->
      let max = max l in
      N (max, delete max l, r)
end
