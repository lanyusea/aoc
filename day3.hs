import System.IO
import Control.Monad
import Debug.Trace
import Data.List
import Data.Maybe

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

---

rm x v p = if x!!p == v then Nothing else Just x

rmByIndex :: [[Int]] -> Int -> Int ->[[Int]]
rmByIndex n v p = mapMaybe (\x -> rm x v p) n 

findMcbRef n = map mostFrequent $ transpose n

findLcbRef n = map leastFrequent $ transpose n

rmLoopByMcb n index | length n == 1 = n
  | otherwise = rmLoopByMcb (rmByIndex n (ref!!index) index) (index + 1) where ref = findMcbRef n

rmLoopByLcb n index | length n == 1 = n
  | otherwise = rmLoopByLcb (rmByIndex n (ref!!index) index) (index + 1) where ref = findLcbRef n

fn n = o2Rate * co2Rate where
    orderedNum = map makeup $ map digits $ c n
    o2Rate = convert $ head $ rmLoopByMcb orderedNum 0
    co2Rate = convert $ head $ rmLoopByLcb orderedNum 0


main = do
    handle <- openFile "./day3.input" ReadMode
    contents <- hGetContents handle

    print $ f $ words contents
    print $ fn $ words contents

    hClose handle

