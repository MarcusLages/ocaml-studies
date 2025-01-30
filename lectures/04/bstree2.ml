type 'a bstree = L | N of 'a * 'a bstree * 'a bstree

let bstree_empty = L

let bstree_is_empty t = t = L

let rec bstree_insert ~cmp x t =
  match t with
  | L -> N (x, L, L)
  | N (v, l, r) when cmp x v < 0 ->
      N (v, bstree_insert ~cmp x l, r)
  | N (v, l, r) when cmp x v > 0 ->
      N (v, l, bstree_insert ~cmp x r)
  | _ -> t

let bstree_of_list ~cmp l =
  List.fold_left (Fun.flip (bstree_insert ~cmp)) bstree_empty l

let rec bstree_max t =
  match t with
  | L -> failwith "bstree_max: empty tree"
  | N (x, _, L) -> x
  | N (_, _, r) -> bstree_max r

let rec bstree_delete ~cmp x t =
  match t with
  | L -> L
  | N (v, l, r) when cmp x v < 0 ->
      N (v, bstree_delete ~cmp x l, r)
  | N (v, l, r) when cmp x v > 0 ->
      N (v, l, bstree_delete ~cmp x r)
  | N (_, l, L) -> l
  | N (_, L, r) -> r
  | N (_, l, r) ->
      let max = bstree_max l in
      N (max, bstree_delete ~cmp max l, r)
