module type Monad = sig
    type 'a t

    (* Functor property:
        Mapping one type (category) to another one using
        properties from its elements
    *)
    val fmap: 'a t -> ('a -> 'b) -> 'b t
    val (>>|): 'a t -> ('a -> 'b) -> 'b t
    val (<$>): 'a t -> ('a -> 'b) -> 'b t

    (* Applicative property:
        A functor
    *)
    val pure: 'a -> 'a t
    val apply: 'a t -> ('a -> 'b) t -> 'b t
    val (<*>): 'a t -> ('a -> 'b) t -> 'b t

    (* Monad functor:
        An Applicative functor that applies functions to wrapped objects
        without unwrapping them
    *)
    val return: 'a -> 'a t
    val bind: 'a t -> ('a -> 'b t) -> 'b t
    val (>>=): 'a t -> ('a -> 'b t) -> 'b t
    val compose: ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
    val (>=>): ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
end

module Maybe: Monad = struct
    type 'a t = 'a option

    let pure x = Some x
    let return = pure

    let fmap mx f =
        match mx with
        | None -> None
        | Some x -> Some (f x)

    let (>>|) = fmap
    let (<$>) = fmap

    let pure x = Some x
    let apply apx apf = 
        match apx, apf with
        | Some x, Some f -> pure (f x)
        | _ -> None

    let (<*>) = apply
    
    let return = pure
    let bind mx f =
        match mx with
        | None -> None
        | Some x -> f x     
    let (>>=) = bind
    let (let*) = bind

    let compose f g x =
        f x >>= fun y ->
            g y
    let compose f g x =
        let* y = f x in 
        g y
    let (>=>) = compose
end