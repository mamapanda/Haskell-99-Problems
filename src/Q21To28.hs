module Q21To28 where

import Control.Arrow
import Data.List

import System.Random (newStdGen, randomRs, randomRIO)


-- | Question 21
insertAt::a -> [a] -> Int -> [a]
insertAt x xs i = take (i - 1) xs ++ [x] ++ drop (i - 1) xs

-- | Question 22
range::Int -> Int -> [Int]
range i j = [i..j]

range'::Int -> Int -> [Int]
range' i j = unfoldr buildRange i
  where buildRange n
          | n > j = Nothing
          | otherwise = Just (n, n + 1)

-- | Question 23
rndSelect::[a] -> Int -> IO [a]
rndSelect xs n = map (xs!!) <$> randIndices
  where randIndices = take n . nub <$> randIndices'
        randIndices' = randomRs (0, length xs - 1) <$> newStdGen

-- | Question 24
diffSelect::Int -> Int -> IO [Int]
diffSelect n m = rndSelect [1..m] n

-- | Question 25
rndPermu::[a] -> IO [a]
rndPermu xs = (perms!!) <$> i
  where perms = permutations xs
        i = randomRIO (0, length perms - 1)

rndPermu'::[a] -> IO [a]
rndPermu' xs = rndSelect xs (length xs)

-- | Question 26
combinations::Int -> [a] -> [[a]]
combinations n = filter ((== n) . length) . subsequences

-- | Question 27
group3::Eq a => [a] -> [[[a]]]
group3 = groupN [2, 3, 4]

groupN::Eq a => [Int] -> [a] -> [[[a]]]
groupN [] _ = [[]]
groupN (size:sizes) xs = [c:g | c <- combinations size xs, g <- groupN sizes (xs\\c)]

-- | Question 28
lsort::[[a]] -> [[a]]
lsort = sortOn length

lfsort::[[a]] -> [[a]]
lfsort xss = sortOn lenFreq xss
  where lenFreq xs = snd . head $ filter ((== length xs) . fst) lenFreqs
        lenFreqs = map (head &&& length) . group . sort $ map length xss
