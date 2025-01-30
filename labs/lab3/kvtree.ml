(* Binary search tree *)
(** [kvtree] is a binary search tree for tuples;
    The tuple has the format of a (key, value) pair;
    Format of the kvtree [Node] is: (key, value), left_node, right_node *)
type ('k, 'v) kvtree = Leaf | Node of ('k * 'v) * ('k, 'v) kvtree * ('k, 'v) kvtree

(** [kvtree_empty] represents an empty [kvtree] *)
let kvtree_empty = Leaf

(** [kvtree_is_empty t] returns [true] if [t] is empty,
    false otherwise
*)
let kvtree_is_empty t = t = Leaf

(** [kvtree_insert ~cmp k v t] returns a [kvtree] with a tuple
    [(k, v)] inserted into [t] according to the comparing function
    [~cmp]
*)
let rec kvtree_insert ~cmp k v t =
    match t with
    | Leaf -> Node ((k, v), Leaf, Leaf)
    | Node ((k', v'), l, r) when cmp k k' < 0 ->
        Node ((k', v'), (kvtree_insert ~cmp k v l), r)
    | Node ((k', v'), l, r) when cmp k k' > 0 ->
        Node ((k', v'), l, (kvtree_insert ~cmp k v r))
    | Node (_, l, r) ->
        Node ((k, v), l, r)

(** [kvtree_of_list ~cmp l] returns a [kvtree] generated
    from a list [l] of tuples in the format [(key, value)] using
    a function [~cmp] to position the nodes
*)
let kvtree_of_list ~cmp l =
    let aux acc x =
        let k, v = x
        in
        kvtree_insert ~cmp k v acc
    in
    List.fold_left aux kvtree_empty l

(** [kvtree_find_opt ~cmp k t] returns a value from the
    [t] based on its key [k] and using [~cmp]
    to find it using the right comparisons
*)
let rec kvtree_find_opt ~cmp k t =
    match t with
    | Leaf -> None
    | Node ((k', _), l, r) when cmp k k' < 0 ->
        kvtree_find_opt ~cmp k l
    | Node ((k', _), l, r) when cmp k k' > 0 ->
        kvtree_find_opt ~cmp k r
    | Node ((_, v'), _, _) -> Some v'

(** [kvtree_max t] returns the "largest" [(key, value)]
    tuple inside [t]
*)
let rec kvtree_max t =
    match t with
    | Leaf -> failwith "bstree_max: empty tree"
    | Node (p, _, Leaf) -> p
    | Node (_, _, r) -> kvtree_max r
  
(** [kvtree_delete ~cmp k t] returns a [kvtree] with the [Node]
    that contains the key [k] deleted from [t]
*)
let rec kvtree_delete ~cmp k t =
    match t with
    | Leaf -> Leaf
    | Node ((k', v'), l, r) when cmp k k' < 0 ->
        Node ((k', v'), (kvtree_delete ~cmp k l), r)
    | Node ((k', v'), l, r) when cmp k k' > 0 ->
        Node ((k', v'), l, (kvtree_delete ~cmp k r))
    | Node (_, l, Leaf) ->
        l
    | Node (_, Leaf, r) ->
        r
    | Node (_, l, r) ->
        let k', v' = kvtree_max l in
        Node ((k', v'), kvtree_delete ~cmp k' l, r)

(**/**)
let test_kvtree () =
    let t = kvtree_of_list ~cmp:Int.compare [(1,'a'); (4,'b'); (2, 'c'); (-3, 'd'); (23, 'e')]
    in assert (t = Node ((1, 'a'), Node ((-3, 'd'), Leaf, Leaf),
                   Node ((4, 'b'), Node ((2, 'c'), Leaf, Leaf), Node ((23, 'e'), Leaf, Leaf))));
    assert (kvtree_is_empty kvtree_empty = true);
    assert (kvtree_is_empty Leaf = true);
    assert (kvtree_is_empty t = false);
    assert (kvtree_find_opt ~cmp:Int.compare 2 t = Some 'c');
    assert (kvtree_find_opt ~cmp:Int.compare 2234234 t = None);
    assert (kvtree_insert ~cmp:Int.compare 0 'h' t = 
        Node ((1, 'a'), Node ((-3, 'd'), Leaf, Node ((0, 'h'), Leaf, Leaf)),
        Node ((4, 'b'), Node ((2, 'c'), Leaf, Leaf), Node ((23, 'e'), Leaf, Leaf))));
    assert (kvtree_insert ~cmp:Int.compare 1 'h' t =
        Node ((1, 'h'), Node ((-3, 'd'), Leaf, Leaf),
        Node ((4, 'b'), Node ((2, 'c'), Leaf, Leaf), Node ((23, 'e'), Leaf, Leaf)))); 
    assert (kvtree_delete ~cmp:Int.compare 1 t =
        Node ((-3, 'd'), Leaf,
        Node ((4, 'b'), Node ((2, 'c'), Leaf, Leaf), Node ((23, 'e'), Leaf, Leaf))));
    assert (kvtree_delete ~cmp:Int.compare 134134 t = t);
    assert (kvtree_delete ~cmp:Int.compare 23 t =
    Node ((1, 'a'), Node ((-3, 'd'), Leaf, Leaf),
    Node ((4, 'b'), Node ((2, 'c'), Leaf, Leaf), Leaf)))
(**/**)