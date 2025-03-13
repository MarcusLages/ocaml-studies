type 'a t
exception Empty
val empty : 'a list
val is_empty : 'a list -> bool
val enqueue : 'a -> 'a list -> 'a list
val dequeue : 'a list -> 'a list
val front : 'a list -> 'a
val front_opt : 'a list -> 'a option

(* True: val of_list : 'a -> 'a
   Modified to restrict the type:
*)
val of_list : 'a list -> 'a t
