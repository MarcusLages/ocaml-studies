type 'a t
exception Empty

val empty : 'a t
val is_empty : 'a t -> bool
val enqueue : 'a -> 'a t -> 'a t
val dequeue : 'a t -> 'a t
val front : 'a t -> 'a
val front_opt : 'a t -> 'a option
val of_list : 'a list -> 'a t

