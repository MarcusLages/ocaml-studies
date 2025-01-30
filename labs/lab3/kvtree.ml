(* Binary search tree *)
type ('k, 'v) kvtree = Leaf | Node of ('k * 'v) * ('k, 'v) kvtree * ('k, 'v) kvtree
let kvtree_empty = Leaf
let kvtree_is_empty t = t = Leaf

(* let rec kvtree_size t =
    match t with
    | Leaf -> 0
    | Node (_, l, r) -> 1 + kvtree_size l + kvtree_size r *)

let rec kvtree_insert ~cmp k v t =
    match t with
    | Leaf -> Node ((k, v), Leaf, Leaf)
    | Node ((k', v'), l, r) when cmp k k' < 0 ->
        Node ((k', v'), (kvtree_insert ~cmp k v l), r)
    | Node ((k', v'), l, r) when cmp k k' > 0 ->
        Node ((k', v'), l, (kvtree_insert ~cmp k v r))
    | Node (_, l, r) ->
        Node ((k, v), l, r)

let kvtree_of_list ~cmp l =
    let aux acc x =
        let k, v = x
        in
        kvtree_insert ~cmp k v acc
    in
    List.fold_left aux kvtree_empty l

(* Why does the type not work? *)
let rec kvtree_find_opt ~cmp k t =
    match t with
    | Leaf -> None
    | Node ((k', _), l, r) when cmp k k' < 0 ->
        kvtree_find_opt ~cmp k l
    | Node ((k', _), l, r) when cmp k k' > 0 ->
        kvtree_find_opt ~cmp k r
    | Node ((_, v'), _, _) -> Some v'

let rec kvtree_max t =
    match t with
    | Leaf -> failwith "bstree_max: empty tree"
    | Node (p, _, Leaf) -> p
    | Node (_, _, r) -> kvtree_max r
  
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

let t = kvtree_of_list ~cmp:Int.compare [(1,'a'); (4,'b'); (2, 'c'); (-3, 'd'); (23, 'e')]