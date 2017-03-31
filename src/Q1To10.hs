module Q1To10 where

import Control.Arrow
import Data.List

-- | Question 1
myLast::[a] -> a
myLast = last

myLast'::[a] -> a
myLast' [] = error "Q1"
myLast' [x] = x
myLast' (_:xs) = myLast' xs

-- | Question 2
myButLast::[a] -> a
myButLast = last . init

-- | Question 3
elementAt::[a] -> Int -> a
elementAt xs i = xs!!(i - 1)

-- | Question 4
myLength::[a] -> Int
myLength = length

myLength'::[a] -> Int
myLength' = foldl (\len _ -> len + 1) 0

-- | Question 5
myReverse::[a] -> [a]
myReverse = reverse

myReverse'::[a] -> [a]
myReverse' = foldl (flip (:)) []

-- | Question 6
isPalindrome::Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- | Question 7
data NestedList a = Elem a | List [NestedList a]

flatten::NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

-- | Question 8
compress::Eq a => [a] -> [a]
compress = map head . group

-- | Question 9
pack::Eq a => [a] -> [[a]]
pack = group

pack'::Eq a => [a] -> [[a]]
pack' [] = []
pack' xs = sublist:pack' rest
  where (sublist, rest) = span (== head xs) xs

-- | Question 10
encode::Eq a => [a] -> [(Int, a)]
encode = map (length &&& head) . pack
