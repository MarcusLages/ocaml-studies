from n = n : from (n + 1)

-- take is already implemented
nats = from 1

sieve (h:t) = h : sieve [a | a <- h, a `mod` h /= 0]
primes = sieve [2..]

-- Function without recursion that uses a list automatically becomes a stream
fib a b = a :  fib b (a + b)

fibs = fib 0 1
    where
        fib a b = a :  fib b (a + b)

fibs' = 0 : 1 : zipWith (+) fibs' (tail fibs')



-- Show is a type class (kind of like interface)
-- Show a => a -> String
show "hello"
show 2

fizzbuzz start finish =
    fb n
    | n `rem` 15 == 0 = "fizzbuzz"
    | n `rem` 5 == 0 = "buzz"
    | n `rem` 3 == 0 = "fizz"
    | otherwise = show n
    where
    if start > finish then return ()
    else do
        putStrLn $ fb start
        fizzbuzz (start + 1) finish

-- No need for the star
-- Constructor names are also functions
data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving Show

insert x Leaf = Node x Leaf Leaf
insert x t@(Node x' l r) -- t@ works just as: "() as t" in OCaml
    | x < x' = Node x' (insert x l) r
    | x > x' = Node x' l (insert x r)
    | otherwise = t

fromLIst l = foldl (flip insert) Leaf l

size Leaf = 0
size (Node _ l r) = 1 + size l + size r

largest Leaf = error "largest: empty tree"
largest (Node x _ Leaf) = x'
largest (Node _ _ r) = largest r

delete _ Leaf = Leaf
delete x t@(Node x' l r)
    | x < x' = Node x' (delete x l) r
    | x > x' = Node x' l (delete x r)
    | otherwise = deleteRoot t
    where
        deleteRoot (Node _ l Leaf) = l
        deleteRoot (Node _ Leaf r) = r
        deleteRoot (Node _ l _) =
            let m = largest l in
            Node m (delete m l) r