module Main where
import Data.Char
import Data.Ord
import Data.List
import System.Environment

main = do
    [file] <- getArgs
    text <- readFile file
    putStrLn show $ take 10 $ countWords text

countWords text =
    sortBy (comparing (Down . snd)) $ 
    map (\l -> (head l, length l)) $
    group  $ sort $ words text

sortBy (Down . snd) $ map (\l -> (head l, length l)) $ group $ sort $ words text