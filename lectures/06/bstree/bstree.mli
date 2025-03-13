type 'a t  (* abtract *)
(* type 'a t = Leaf | Node of 'a * 'a t * 'a t *)

val empty : 'a t
val is_empty : 'a t -> bool
val size : 'a t -> int
val insert : 'a -> 'a t -> 'a t
val of_list : 'a list -> 'a t
val mem : 'a -> 'a t -> bool
val delete : 'a -> 'a t -> 'a t

