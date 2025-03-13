type 'a t = Leaf | Node of 'a * 'a t * 'a t

let empty = Leaf

let is_empty t = t = Leaf

let rec size t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + size l + size r

let rec insert x t =
  match t with
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (v, l, r) when x < v ->
      Node (v, insert x l, r)
  | Node (v, l, r) when x > v ->
      Node (v, l, insert x r)
  | _ -> t

let of_list l =
  List.fold_left (Fun.flip insert) empty l

let rec mem x t =
  match t with
  | Leaf -> false
  | Node (v, l, _) when x < v ->
      mem x l
  | Node (v, _, r) when x > v ->
      mem x r
  | _ -> true


let rec max t =
  match t with
  | Leaf -> failwith "max: empty tree"
  | Node (v, _, Leaf) -> v
  | Node (_, _, r) -> max r

let rec delete x t =
  match t with
  | Leaf -> Leaf
  | Node (v, l, r) when x < v ->
      Node (v, delete x l, r)
  | Node (v, l, r) when x > v ->
      Node (v, l, delete x r)
  | Node (_, l, Leaf) -> l
  | Node (_, Leaf, r) -> r
  | Node (_, l, r) ->
      let max = max l in
      Node (max, delete max l, r)

