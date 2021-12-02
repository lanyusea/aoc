import System.IO
import Control.Monad
import Debug.Trace

c :: [String] -> [Int]
c = map read

f a = calc 0 a
    where calc sum (a':b':xs)
            | null xs == True = if b'>a' then sum +1 else sum
            | b' > a' = trace (show b' ++ ">" ++ show a') $ calc (sum+1) (b':xs)
            | otherwise = trace (show b' ++ "<" ++ show a') $ calc sum (b':xs)

fn a = calc 0 a
    where calc sum (a':b':c':d':xs)
            | null xs == True = if b'+c'+d'>a'+b'+c' then sum +1 else sum
            | b'+c'+d'>a'+b'+c' = calc (sum+1) (b':c':d':xs)
            | otherwise = calc sum (b':c':d':xs)
main = do
    handle <- openFile "./day1.input" ReadMode
    contents <- hGetContents handle
    print $ f $ c $ words contents
    print $ fn $ c $ words contents

    hClose handle

