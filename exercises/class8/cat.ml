(* Category theory
    Sets + arrows
    Arrows (or morphisms) are the functions
    Morphisms/functions can be composed and the composition
    must be associative
    Morphisms that map a set/state to itself is called
    the identity

    Cat theory functor (not the same as before)
    Functor is a type constructor
    list is a type constructor (for example)

    (* t is a constructor *)
    fmap or <$>: ('a -> 'b) -> 'a t -> 'b t
    If we can define an fmap for the type constructor, then
    it's a functor

*)

let (<$>) f mx =
    match mx with
    | None -> None
    | Some x -> Some (f x)

let square x = x * x
(* let error = square (Some 2) *)
let s2 = square <$> Some 2

(* Doesn't work *)
let add x y = x + y

(* 
    Applicative functor
    fmap or <*>: ('a -> 'b) t -> 'a t -> 'b t
*)
let (<*>) mf mx =
    match mf with
    | None -> None
    | Some f -> f <$> mx

let r5 = add <$> Some 2 <*> Some 3

(* 
    add: 'a -> 'a -> 'a
    add': 'a t -> 'a t -> 'a t

    from 'a -> 'b -> 'c to
    lift2 f: 'a t -> 'b t -> 'c t
    
    Monad property: 
    bind or (>>=): 'a t -> ('a -> 'b t) -> 'b t
*)