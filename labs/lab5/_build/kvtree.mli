module type OrderedType = sig
    type t
    val compare : t -> t -> int
end
module type S =
  sig
    type k
    type 'a t = L | N of (k * 'a) * 'a t * 'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val insert : k -> 'a -> 'a t -> 'a t
    val find: k -> 'a t -> 'a
    val find_opt : k -> 'a t -> 'a option
    val delete : k -> 'a t -> 'a t
    val of_list : (k * 'a) list -> 'a t
    val to_list : 'a t -> (k * 'a) list
    val to_string : ((k * 'a) -> string) -> 'a t -> string
end
module Make(Ord : OrderedType): S with type k = Ord.t