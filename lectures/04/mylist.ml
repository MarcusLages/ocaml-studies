type 'a mylist = Nil | Cons of 'a * 'a mylist  (* recursive definition *)

let rec length = function
  | Nil -> 0
  | Cons (_, t) -> 1 + length t

let rec map f l =
  match l with
  | Nil -> Nil
  | Cons (h, t) -> Cons (f h, map f t)
