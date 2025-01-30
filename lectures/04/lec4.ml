(* examples of sum types *)
type 'a option = None | Some of 'a

type ('a, 'b) result = Ok of 'a | Error of 'b

type direction = North | East | South | West

type suit = Club | Diamond | Heart | Spade

type rank = Num of int | Jack | Queen | King | Ace

let rank_compare r1 r2 =
  match r1, r2 with
  | Num x1, Num x2 -> Int.compare x1 x2
  | Num _, _ -> -1
  | _, Num _ -> 1
  | _, _ -> Stdlib.compare r1 r2

type card = rank * suit

let card_compare (r1, s1) (r2, s2) =
  let c = rank_compare r1 r2 in
  if c = 0 then Stdlib.compare s1 s2 else c
