module Q46To50 where

import Control.Arrow
import Data.List
import Data.Ord

-- | Question 46
and'::Bool -> Bool -> Bool
and' = (&&)

or'::Bool -> Bool -> Bool
or' = (||)

nand'::Bool -> Bool -> Bool
nand' a b = not $ and' a b

nor'::Bool -> Bool -> Bool
nor' a b = not $ or' a b

xor'::Bool -> Bool -> Bool
xor' = (/=)

impl'::Bool -> Bool -> Bool
impl' a = or' (not a)

equ'::Bool -> Bool -> Bool
equ' = (==)

table::(Bool -> Bool -> Bool) -> IO ()
table p = putStrLn . unlines $ map (unwords . map show) table'
  where table' = [[a, b, p a b] | a <- [True, False], b <- [True, False]]

-- | Question 47
infixl 8 `equ'`
infixl 7 `and'`
infixl 7 `nand'`
infixl 6 `xor'`
infixl 5 `or'`
infixl 5 `nor'`
infixl 4 `impl'`

table2::(Bool -> Bool -> Bool) -> IO ()
table2 = table

-- | Question 48
tablen::Int -> ([Bool] -> Bool) -> IO ()
tablen n p = printTable [row ++ [p row] | row <- tablen' n]
  where tablen' 1 = map return [True, False]
        tablen' n' = [a:row | a <- [True, False], row <- tablen' (n' - 1)]
        printTable = putStrLn . unlines . map (unwords . map show)

-- | Question 49
gray::Int -> [String]
gray 1 = ["0", "1"]
gray n = map ('0':) bits ++ map ('1':) (reverse bits)
  where bits = gray (n - 1)

-- | Question 50
huffman::[(Char, Int)] -> [(Char, String)]
huffman = huffman' . sortOn snd .  map (first (return . (id &&& const "")))
  where huffman' [] = []
        huffman' [x] = sortOn fst $ fst x
        huffman' (x:x':xs) = huffman' $ insertBy (comparing snd) (combine x x') xs
        combine (pairs, w) (pairs', w') = (addChr '0' pairs ++ addChr '1' pairs', w + w')
        addChr c' = map (second (c':))
