type 'a heap = L | N of 'a * 'a heap * 'a heap

let rec merge h1 h2 =
  match h1, h2 with
  | L, h 
  | h, L -> h
  | N (v1, l1, r1), N (v2, l2, r2) ->
      if v1 >= v2 then N (v1, r1, merge l1 h2)
      else N (v2, r2, merge h1 l2)

let insert x h = merge (N (x, L, L)) h

let return_max = function
  | L -> None
  | N (v, _, _) -> Some v

let delete_max = function
  | L -> L
  | N (_, l, r) -> merge l r
