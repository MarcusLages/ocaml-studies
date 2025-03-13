(* Abstract version: *)
type 'a t

(* Concrete version *)
(* type 'a t = Leaf | Node of 'a * 'a t * 'a t *)

val empty : 'a t
val is_empty : 'a t -> bool
val size : 'a t -> int
val insert : 'a -> 'a t -> 'a t
val of_list : 'a list -> 'a t
val search : 'a -> 'a t -> bool
val max : 'a t -> 'a
val delete : 'a -> 'a t -> 'a t
