(* motivation for maybe monad *)
type 'a bstree = L | N of 'a * 'a bstree * 'a bstree

let empty = L

let rec insert x t =
  match t with
  | L -> N (x, L, L)
  | N (x', l, r) ->
     if x < x' then N (x', insert x l, r)
     else if x > x' then N (x', l, insert x r)
     else t

let of_list l = List.fold_left (Fun.flip insert) empty l

let left t =
  match t with
  | L -> None
  | N (_, l, _) -> Some l

let right t =
  match t with
  | L -> None
  | N (_, _, r) -> Some r

(* how do we combine left and right to get, e.g., left_right ? *)  

(* maybe monad - 2 functions *)
let return x = Some x

let bind mx f =
  match mx with
  | None -> None
  | Some x -> f x

let ( >>= ) = bind

(* e.g., return t >>= left >>= right  (for tree t) *)

let sqrt t =
  if t < 0. then None
  else Some (Stdlib.sqrt t)

(* 
   how do we find the 4th root of 3. by applying sqrt twice:
   return 3. >>= sqrt >>= sqrt
*)  
