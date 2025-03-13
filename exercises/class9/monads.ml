(* Maybe monad *)
let return x = Some x

let bind mx f =
    match mx with
    | None -> None
    | Some x -> f x
    
let (>>=) = bind

(* Monad is a functor and an applicative functor at the same time
    (Monad) bind (>>=): 'a t -> ('a -> 'b t) -> 'b t
    (Functor) fmap <$>: ('a -> 'b) -> 'a t -> 'b t
    (Applicative functor) pure <*>: ('a -> 'b) t -> 'a t -> 'b t
*)

(* Lift: *)
let lift f mx =
    mx >>= fun x -> return (f x)

(* Lift2: *)
let lift2 f mx my =
    mx >>= fun x ->
    my >>= fun y ->
        return (f x y)

let compose f g x =
    f x >>= fun y -> g y

let (>=>) = compose

(* Presentations topics:
    continuations
    scheme
    elm (mario game)
    reasonML
    F#
    y-combinator / lambda calculus
    polymorphic recursion
    algebraic effect handler
    GADTs (general algebraic data types)
    phanatom type / existential type
*)