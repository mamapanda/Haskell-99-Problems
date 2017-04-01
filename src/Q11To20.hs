module Q11To20 where

import Q01To10 (encode)

-- | Question 11
data EncodeUnit a = Single a | Multiple Int a
                  deriving (Show)

encodeModified::Eq a => [a] -> [EncodeUnit a]
encodeModified = map encodeModified' . encode
  where encodeModified' (n, x)
          | n == 1 = Single x
          | otherwise = Multiple n x

-- | Question 12
decodeModified::[EncodeUnit a] -> [a]
decodeModified = concatMap decodeModified'
  where decodeModified' (Single x) = [x]
        decodeModified' (Multiple n x) = replicate n x

-- | Question 13
encodeDirect::Eq a => [a] -> [EncodeUnit a]
encodeDirect xs = map toEncodeUnit $ foldr encodeDirect' [] xs
  where toEncodeUnit (y, 1) = Single y
        toEncodeUnit (y, n) = Multiple n y
        encodeDirect' y [] = [(y, 1)]
        encodeDirect' y (acc@((z, count):zs))
          | y == z = (z, count + 1):zs
          | otherwise = (y, 1):acc

-- | Question 14
dupli::[a] -> [a]
dupli = concatMap (\x -> [x, x])

-- | Question 15
repli::[a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- | Question 16
dropEvery::[a] -> Int -> [a]
dropEvery xs n = [x | (x, i) <- zip xs [(1::Int)..], rem i n /= 0]

-- | Question 17
split::[a] -> Int -> ([a], [a])
split xs n = foldr split' ([], []) $ zip xs [1..]
  where split' (x, i) (left, right)
          | i <= n = (x:left, right)
          | otherwise = (left, x:right)

-- | Question 18
slice::[a] -> Int -> Int -> [a]
slice xs i j = take (j - i + 1) $ drop (i - 1) xs

-- | Question 19
rotate::[a] -> Int -> [a]
rotate xs n = drop n' xs ++ take n' xs
  where n' = if n >= 0 then n else length xs + n

-- | Question 20
removeAt::Int -> [a] -> (a, [a])
removeAt i xs = (xs!!(i - 1), take (i - 1) xs ++ drop i xs)
