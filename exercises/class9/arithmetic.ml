let return x = Some x

let bind mx f =
    match mx with
    | None -> None
    | Some x -> f x
    
let (>>=) = bind
let (let*) = bind

let div a b =
    if b = 0 then None
    else Some (a/b)

(* How do we make the other arithmetic operations take optional values
    and return optional values? *)

let square' x = x * x (* Takes int, returns an int *)
let square'' x = Some (x * x) (* Takes int, returns an int option *)

let square mx = (* Takes int option, returns int option*)
    mx >= square'

(* Takes int option, returns int option *)
let square mx = mx >>= fun x -> return (x * x)

(* Lift: *)
let lift f mx =
    mx >>= fun x -> return (f x)

let cube' x = x * x * x
let cube = lift cube'

(* Monad is a functor and an applicative functor at the same time
    (Functor) fmap <$>: ('a -> 'b) -> 'a t -> 'b t
    (Applicative functor) pure <*>: ('a -> 'b) t -> 'a t -> 'b t
*)

(* let add' x y = x + y
    let add' = fun x -> fun y -> x + y
*)
let add mx my = mx >>= fun x -> my >>= fun y -> return (x + y)

(* Lift2: *)
let lift2 f mx my =
    mx >>= fun x ->
        my >>= fun y ->
            return (f x y)

let ( + ) = lift2 Stdlib.( + )
let ( - ) = lift2 Stdlib.( - )
let ( * ) = lift2 Stdlib.( * )
let ( / ) mx my =
    mx >>= fun x ->
        my >>= fun y ->
            div x y
let ( / ) mx my =
    let* x = mx in
    let* y = my in
        div x y