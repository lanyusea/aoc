import System.IO
import Control.Monad
import Debug.Trace
import Data.List

c :: [String] -> [Int]
c = map read 

digits :: Int -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

repeatNTimes :: Int -> a -> [a]
repeatNTimes 0 _ = []
repeatNTimes n x = x : repeatNTimes (n - 1) x

makeup n | length n == 12 = n
          | otherwise = repeatNTimes (12 - length n) 0 ++ n

mostFrequent :: [Int] -> Int
mostFrequent ns =
  snd (maximum [ (length ks, head ks) | ks <- group (sort ns) ])

leastFrequent :: [Int] -> Int
leastFrequent ns =
  snd (minimum [ (length ks, head ks) | ks <- group (sort ns) ])

convert :: [Int] -> Int
convert n = calc 0 n 
    where calc r' (n':xs)
                | null xs == True = r' * 2 + n'
                | otherwise = calc (r' * 2 + n') xs

f n  = gammaRate * epsilonRate where
    orderedNum = transpose $ map makeup $ map digits $ c n
    gammaRate = convert $ map mostFrequent orderedNum
    epsilonRate = convert $ map leastFrequent orderedNum


 
main = do
    handle <- openFile "./day3.input" ReadMode
    contents <- hGetContents handle
    print $ f $ words contents

    hClose handle

