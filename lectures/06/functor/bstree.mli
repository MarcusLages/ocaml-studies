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

module Make(Ord : OrderedType) : S with type elt = Ord.t
