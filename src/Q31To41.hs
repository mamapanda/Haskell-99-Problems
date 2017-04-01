module Q31To41 where

import Control.Arrow
import Data.List

-- | Question 31
isPrime::Int -> Bool
isPrime n = n == 2 || (n /= 1 && odd n && isPrime' [3,5..])
  where isPrime' [] = True -- should never even get here
        isPrime' (x:xs) = x * x > n || (rem n x /= 0 && isPrime' xs)

-- | Question 32
myGCD::Int -> Int -> Int
myGCD a b
  | b == 0 = abs a
  | otherwise = myGCD b (rem a b)

-- | Question 33
coprime::Int -> Int -> Bool
coprime a = (== 1) . gcd a

-- | Question 34
totient::Int -> Int
totient 1 = 1
totient n = length $ filter (coprime n) [1..(n - 1)]

-- | Question 35
primeFactors::Int -> [Int]
primeFactors n = reverse $ primeFactors' n 2 []
  where primeFactors' n' i factors
          | n' == 1 = factors
          | rem n' i == 0 = primeFactors' (quot n' i) i (i:factors)
          | otherwise = primeFactors' n' (i + 1) factors

-- | Question 36
primeFactorsMult::Int -> [(Int, Int)]
primeFactorsMult = map (head &&& length) . group . primeFactors

-- | Question 37
totient'::Int -> Int
totient' = product . map formula . primeFactorsMult
  where formula (p, m) = (p - 1) * p ^ (m - 1)

-- | Question 39
primesR::Int -> Int -> [Int]
primesR i j = filter isPrime [i..j]

-- | Question 40
goldbach::Int -> (Int, Int)
goldbach n = head [(a, b)
                  | a <- primesR 1 n
                  , let b = n - a
                  , isPrime b
                  ]

-- | Question 41
goldbachList::Int -> Int -> [(Int, (Int, Int))]
goldbachList i j = map (id &&& goldbach) $ filter even [i..j]

goldbachList'::Int -> Int -> Int -> [(Int, (Int, Int))]
goldbachList' i j n = filter (\(_, (a, b)) -> a > n && b > n) $ goldbachList i j
