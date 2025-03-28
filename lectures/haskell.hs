f::a->a
-- Constraint on f
f :: Num a => a -> a -> a
:type 1

-- Type class
-- Used to overload operators and functions
-- * usually stands for a type

-- Kind of a type
-- Tree Int and Tree String are types of kind Tree
Tree a

-- Section
(2/) 3
(/2) 3
(+) 1 2

fact n =
    if <= 0 then 1
    else n * fact (n - 1)

-- Int != Integer
-- Int is 32/64b
-- Integer is any number of any amount of Bytes to avoid overflows

-- Type constructor also starts in uppercase letter

-- putStrLn "hello" only ghci
module Main where
    -- IO monad
    -- Separate the IO from the "functional world"
    -- main = putStrLn "Hello world" -- String -> IO ()
    main = do
        -- Input is wrapped in a monad and "do exctr <- monad" is used
        -- to extract that value
        name <- getLine -- IO String
        -- putStrLn ("hell" ++ name) -- ++ is for lists
        putStrLn ("hell" <> name)    -- <> is for any Semigroup
