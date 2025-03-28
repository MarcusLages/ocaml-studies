length1 l =
    case l of -- case statement
        [] -> 0
        (_:t) -> 1 + length1 t

{- Using clauses, working like an overloaded pattern matching
 - Indentation important
 -}
length2 l [] = 0
length2 (_:t) = 1 + length2 t

-- :l to use a file
length1 [1,2,3,4]

-- range
[1..10]
[1,3..10]
-- Infinite range. Since haskell is lazy, you don't have to worry,
-- Just don't use it on the shell
[1..]

[x * x | x <- [1..20]]
[x * x | x <- [1..20], (x * x) `mod` 3 /= 0]
[x * x | x <- [1..20], let square = (x * x), square `mod` 3 /= 0]

let a = 1; b = 2 in a + b

-- Indentation is important; the layout rule
let a = 1
    b = 2
in a + b

qsort [] = []
qsort (h:t) =
    qsort [y | y <- t, y < h] <> [h] <> [y | y <- t, y >= h]

{- QSORT in OCaml
let qsort = function
| [] -> []
| h::t ->
  qsort (List.filter (fun x -> x < h) t) @ [h] @ qsort (List.filter (fun x -> x >= h) t);;

-}