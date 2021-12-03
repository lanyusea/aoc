import System.IO
import Control.Monad
import Debug.Trace

f a = calc 0 0 a
    where calc forward up (a':b':xs)
            | null xs == True = (forward + read b') * up -- checked the dataset, the last one is forward
            | a' == "forward" = calc (forward + read b') up xs
            | a' == "up" =  calc forward (up - read b') xs
            | a' == "down" =  calc forward (up + read b') xs

fn a = calc 0 0 0 a 
    where calc forward up aim (a':b':xs)
            | null xs == True = (forward + read b') * (up + aim * read b') 
            | a' == "forward" = calc (forward + read b') (up + aim * read b') aim xs
            | a' == "up" = calc forward up (aim - read b') xs
            | a' == "down" = calc forward up (aim + read b') xs

main = do
    handle <- openFile "./day2.input" ReadMode
    contents <- hGetContents handle
    print $ f $ words contents
    print $ fn $ words contents

    hClose handle

