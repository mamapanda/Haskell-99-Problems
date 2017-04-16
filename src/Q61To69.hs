{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Q61To69 where

import Control.Arrow
import Data.List
import Text.Printf

import Q55To60 (Tree(..))
import Q61To69Trees

-- | Question 61
countLeaves::Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ lhs rhs) = countLeaves lhs + countLeaves rhs

-- | Question 61A
leaves::Tree a -> [a]
leaves Empty = []
leaves (Branch v Empty Empty) = [v]
leaves (Branch _ lhs rhs) = leaves lhs ++ leaves rhs

-- | Question 62
internals::Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch v lhs rhs) = v:internals lhs ++ internals rhs

-- | Question 62B
atLevel::Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch v lhs rhs) level
  | level < 1 = []
  | level == 1 = [v]
  | otherwise = concatMap (`atLevel` (level - 1)) [lhs, rhs]

-- | Question 63
completeBinaryTree::Int -> Tree Char
completeBinaryTree n = completeBinaryTree' 0
  where completeBinaryTree' n'
          | n' >= n = Empty
          | otherwise = let lhs = completeBinaryTree' $ 2 * n' + 1
                            rhs = completeBinaryTree' $ 2 * n' + 2
                        in Branch 'x' lhs rhs

countNodes::Tree a -> Int
countNodes Empty = 0
countNodes (Branch _ lhs rhs) = 1 + countNodes lhs + countNodes rhs

isCompleteBinaryTree::Tree a -> Bool
isCompleteBinaryTree tree = isCompleteBinaryTree' 0 tree
  where nodeCount = countNodes tree
        isCompleteBinaryTree' _ Empty = True
        isCompleteBinaryTree' n (Branch _ lhs rhs)
          | n >= nodeCount = False
          | otherwise = isCompleteBinaryTree' (2 * n + 1) lhs
                        && isCompleteBinaryTree' (2 * n + 2) rhs

-- | Question 64
mapTree::(a -> b) -> Tree a -> Tree b
mapTree f = mapTree'
  where mapTree' Empty = Empty
        mapTree' (Branch v lhs rhs) = Branch (f v) (mapTree' lhs) (mapTree' rhs)

layout64::Tree a -> Tree (a, (Int, Int))
layout64 = fst . layout64' 0 1
  where layout64' _ _ Empty = (Empty, 0)
        layout64' count y (Branch v lhs rhs) =
          let (lhs', lcount) = layout64' count (y + 1) lhs
              x = lcount + count + 1
              (rhs', rcount) = layout64' x (y + 1) rhs
          in (Branch (v, (x, y)) lhs' rhs', lcount + rcount + 1)

-- | Question 65
height::Tree a -> Int
height Empty = 0
height (Branch _ lhs rhs) = 1 + max (height lhs) (height rhs)

xLeft::Tree (a, (Int, Int)) -> Int
xLeft Empty = 1 -- will only be reached with Empty tree
xLeft (Branch (_, (x, _)) Empty _) = x
xLeft (Branch _ lhs _) = xLeft lhs

shiftX::Int -> Tree (a, (Int, Int)) -> Tree (a, (Int, Int))
shiftX _ Empty = Empty
shiftX dx tree = shiftX' tree
  where shiftX' Empty = Empty
        shiftX' (Branch (v, (x, y)) lhs rhs) =
          Branch (v, (x + dx, y)) (shiftX' lhs) (shiftX' rhs)

layout65::Tree a -> Tree (a, (Int, Int))
layout65 Empty = Empty
layout65 tree = shiftX (1 - xLeft tree') tree'
  where tree' = addCoords 0 1 (2 ^ (height tree - 2)) tree
        -- | Makes a tree with x-coordinates relative to root node
        addCoords _ _ _ Empty = Empty
        addCoords x y dx (Branch v lhs rhs) =
          let dx' = quot dx 2
              lhs' = addCoords (x - dx) (y + 1) dx' lhs
              rhs' = addCoords (x + dx) (y + 1) dx' rhs
          in Branch (v, (x, y)) lhs' rhs'

-- | Question 66
zipWith'::(a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ [] [] = []
zipWith' _ xs [] = xs
zipWith' _ [] ys = ys
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys

layout66::Tree a -> Tree (a, (Int, Int))
layout66 tree = shiftX (1 - xLeft tree') tree'
  where (tree', _, _) = layout66' 1 tree
        layout66' _ Empty = (Empty, [], [])
        layout66' y (Branch v lhs rhs) =
          let (lhs', leftxs, rightxs) = layout66' (y + 1) lhs
              (rhs', leftxs', rightxs') = layout66' (y + 1) rhs
              dx = case zipWith (-) rightxs leftxs' of
                     [] -> 1
                     xs -> quot (maximum xs) 2 + 1
              (lhs'', rhs'') = (shiftX (-dx) lhs', shiftX dx rhs')
              leftxs'' = joinLimits leftxs leftxs' dx min
              rightxs'' = joinLimits rightxs rightxs' dx max
          in (Branch (v, (0, y)) lhs'' rhs'', leftxs'', rightxs'')
        joinLimits xs ys dx f = 0:zipWith' f ((+ (-dx)) <$> xs) ((+ dx) <$> ys)

-- | Question 67A
treeToString::Tree Char -> String
treeToString Empty = ""
treeToString (Branch c Empty Empty) = [c]
treeToString (Branch c lhs rhs) =
  printf "%c(%s,%s)" c (treeToString lhs) (treeToString rhs)

stringToTree::String -> Tree Char
stringToTree "" = Empty
stringToTree (c:s) = Branch c (stringToTree lString) (stringToTree rString)
  where (lString, rString) = splitString s
        splitString "" = ("", "")
        splitString s' = splitString' (0::Int) (0::Int) "" (init $ tail s')
        splitString' _ _ _ [] = error "Q67A"
        splitString' lParens rParens lTreeString (c':s')
          | c' == '(' = splitString' (lParens + 1) rParens (c':lTreeString) s'
          | c' == ')' = splitString' lParens (rParens + 1) (c':lTreeString) s'
          | c' == ',' && lParens == rParens = (reverse lTreeString, s')
          | otherwise = splitString' lParens rParens (c':lTreeString) s'

-- | Question 68
treeToPreorder::Tree Char -> String
treeToPreorder Empty = ""
treeToPreorder (Branch c lhs rhs) = c:treeToPreorder lhs ++ treeToPreorder rhs

treeToInorder::Tree Char -> String
treeToInorder Empty = ""
treeToInorder (Branch c lhs rhs) = treeToInorder lhs ++ [c] ++ treeToInorder rhs

preorderToTree::String -> Tree Char
preorderToTree "" = Empty
preorderToTree (c:s) = Branch c (preorderToTree lString) (preorderToTree rString)
  where halfLen = quot (length s) 2
        (lString, rString) = (take halfLen s, drop halfLen s)

preInTree::String -> String -> Tree Char
preInTree preorder inorder = preInTree' $ map (id &&& (`elemIndex` inorder)) preorder
  where preInTree' [] = Empty
        preInTree' ((c, maybeI):preorder') =
          let (lpreorder, rpreorder) = partition ((< maybeI) . snd) preorder'
          in Branch c (preInTree' lpreorder) (preInTree' rpreorder)

-- | Question 69
tree2ds::Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch c lhs rhs) = c:tree2ds lhs ++ tree2ds rhs

ds2tree::String -> Tree Char
ds2tree = fst . ds2tree'
  where ds2tree' "" = (Empty, "")
        ds2tree' ('.':s) = (Empty, s)
        ds2tree' (c:s) =
          let (lhs, s') = ds2tree' s
              (rhs, s'') = ds2tree' s'
          in (Branch c lhs rhs, s'')
