(*  Marcus Vinicius santos Lages
    A01392327
    Set 3-V
*)
module type OrderedType = sig
    type t
    val compare: t -> t -> int
end

module type S = sig
    type k
    type 'a t = L | N of (k * 'a) * 'a t * 'a t

    val empty: 'a t
    val is_empty: 'a t -> bool
    val insert: k -> 'a -> 'a t -> 'a t
    val find: k -> 'a t -> 'a
    val find_opt: k -> 'a t -> 'a option
    val delete: k -> 'a t -> 'a t
    val of_list: (k * 'a) list -> 'a t
    val to_list : 'a t -> (k * 'a) list
    val to_string : ((k * 'a) -> string) -> 'a t -> string
end

(* Binary search tree for key value pairs.*)
module Make(Ord: OrderedType) = struct
    type k = Ord.t
    
    (** [t] is a binary search tree for tuples;
        The tuple has the format of a (key, value) pair;
        Format of the t [N] is: (key, value), left_node, right_node
    *)
    type 'a t = L | N of (k * 'a) * 'a t * 'a t

    (** [empty] represents an empty [t] *)
    let empty = L
    
    (** [is_empty t] returns [true] if [t] is empty,
        [false] otherwise
    *)
    let is_empty t = t = L
    
    (** [insert k v t] returns a [t] with a tuple
        [(k, v)] inserted into [t];
        Uses the function [Ord.compare] passed from the module [Ord]
        passed to the functor to position the nodes
    *)
    let rec insert k v t =
        match t with
        | L -> N ((k, v), L, L)
        | N ((k', v'), l, r) when Ord.compare k k' < 0 ->
            N ((k', v'), (insert k v l), r)
        | N ((k', v'), l, r) when Ord.compare k k' > 0 ->
            N ((k', v'), l, (insert k v r))
        | N (_, l, r) ->
            N ((k, v), l, r)
    
    (** [of_list l] returns a [t] generated
        from a list [l] of tuples in the format [(key, value)];
        Uses the function [Ord.compare] passed from the module [Ord]
        passed to the functor to position the nodes
    *)
    let of_list l =
        let aux acc x =
            let k, v = x
            in
            insert k v acc
        in
        List.fold_left aux empty l
    
    (** [find k t] returns a value from the
        [t] based on its key [k];
        If no value is found a [Failure] is raised
    *)
    let rec find k t =
        match t with
        | L -> failwith "Value not found using this key"
        | N ((k', _), l, r) when Ord.compare k k' < 0 ->
            find k l
        | N ((k', _), l, r) when Ord.compare k k' > 0 ->
            find k r
        | N ((_, v'), _, _) -> v'
    
    (** [find_opt k t] returns a value from the
        [t] based on its key [k] as an [Option]
    *)
    let rec find_opt k t =
        match t with
        | L -> None
        | N ((k', _), l, r) when Ord.compare k k' < 0 ->
            find_opt k l
        | N ((k', _), l, r) when Ord.compare k k' > 0 ->
            find_opt k r
        | N ((_, v'), _, _) -> Some v'
    
    (** [max t] returns the "largest" [(key, value)]
        tuple inside [t]
    *)
    let rec max t =
        match t with
        | L -> failwith "bstree_max: empty tree"
        | N (p, _, L) -> p
        | N (_, _, r) -> max r
      
    (** [delete k t] returns a [t] with the [N]
        that contains the key [k] deleted from [t];
        Uses the function [Ord.compare] passed from the module [Ord]
        passed to the functor to position the nodes
    *)
    let rec delete k t =
        match t with
        | L -> L
        | N ((k', v'), l, r) when Ord.compare k k' < 0 ->
            N ((k', v'), (delete k l), r)
        | N ((k', v'), l, r) when Ord.compare k k' > 0 ->
            N ((k', v'), l, (delete k r))
        | N (_, l, L) ->
            l
        | N (_, L, r) ->
            r
        | N (_, l, r) ->
            let k', v' = max l in
            N ((k', v'), delete k' l, r)

    let to_list t =
        let rec aux acc t =
            match t with
            | L -> acc
            | N ((k, v), l, r) ->
                aux ((k, v)::(aux acc l)) r
        in
        List.rev (aux [] t)
    let rec to_string f t =
        match t with
        | L -> "L"
        | N (pair, l, r) ->
            "N(" ^ f (pair) ^ ", " ^ (to_string f l) ^ ", " ^ (to_string f r) ^ ")"

end