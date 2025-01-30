type 'a btree = Leaf | Node of 'a * 'a btree * 'a btree

let rec size t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + size l + size r

let rec height t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (height l) (height r)

let rec map f t =
  match t with
  | Leaf -> Leaf
  | Node (x, l, r) -> Node (f x, map f l, map f r)

let rec flip t =
  match t with
  | Leaf -> Leaf
  | Node (x, l, r) -> Node (x, r, l)
