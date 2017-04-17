module Q70To73 where

import Control.Arrow
import Data.List
import Text.Printf

data Tree a = Node a [Tree a]
            deriving (Eq)

instance Show a => Show (Tree a) where
  show = showTree ""
    where showTree padding (Node v children) =
            let line = padding ++ label
                padding' = map (const ' ') line
            in line ++ show v ++ "\n" ++ concatMap (showTree padding') children
          label = "|- "

tree1::Tree Char
tree1 = Node 'a' []

tree2::Tree Char
tree2 = Node 'a' [Node 'b' []]

tree3::Tree Char
tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4::Tree Char
tree4 = Node 'b' [ Node 'd' []
                 , Node 'e' []
                 ]

tree5::Tree Char
tree5 = Node 'a' [ Node 'f' [Node 'g' []]
                 , Node 'c' []
                 , Node 'b' [Node 'd' [], Node 'e' []]
                 ]

-- | Question 70C
nnodes::Tree a -> Int
nnodes (Node _ children) = (+ 1) . sum $ map nnodes children

-- | Question 70
treeToString::Tree Char -> String
treeToString (Node c children) = c:concatMap treeToString children ++ "^"

stringToTree::String -> Tree Char
stringToTree "" = error "Q70"
stringToTree s@(c:_) = foldl addNode (Node c []) connections
  where addNode (Node c' children) connection@(cparent, cchild)
          | c' == cparent = Node c' (children ++ [Node cchild []])
          | otherwise = Node c' $ map (`addNode` connection) children
        connections = let s' = foldl connections' "" countsAndIndex
                      in nubBy (\(a, _) (_, b) -> a == b) $ (zip <*> tail) s'
        connections' acc (('^', count), i) = acc ++ [acc!!(i - count - 1)]
        connections' acc ((c', _), _) = acc ++ [c']
        countsAndIndex = map (first (head &&& length)) $ zip (group s) [(0::Int)..]

-- | Question 71
ipl::Tree a -> Int
ipl = ipl' 0
  where ipl' depth (Node _ children) = depth + sum (map (ipl' $ depth + 1) children)

-- | Question 72
bottomUp::Tree Char -> String
bottomUp (Node c children) = concatMap bottomUp children ++ [c]

-- | Question 73
displayLisp::Tree Char -> String
displayLisp (Node c []) = [c]
displayLisp (Node c children) = printf "(%c %s)" c childrenStr
  where childrenStr = unwords $ map displayLisp children

chunkLispTree::String -> [String]
chunkLispTree = chunkLispTree' [] "" (0::Int) (0::Int) . init . tail
  where reverseChunks = reverse . map reverse
        chunkLispTree' chunks "" _ _ "" = reverseChunks chunks
        chunkLispTree' chunks current _ _ "" = reverseChunks $ current:chunks
        chunkLispTree' chunks current lParens rParens s@(c:rest)
          | lParens == 0 && rParens == 0 && c `notElem` " ()" =
              chunkLispTree' ([c]:chunks) "" 0 0 rest
          | lParens /= 0 && lParens == rParens = chunkLispTree' (current:chunks) "" 0 0 s
          | otherwise = chunkLispTree' chunks current' lParens' rParens' rest
          where current' = if c == ' ' && lParens == rParens then current else c:current
                lParens' = if c == '(' then lParens + 1 else lParens
                rParens' = if c == ')' then rParens + 1 else rParens

readLisp::String -> Tree Char
readLisp [c] = Node c []
readLisp s = Node c $ map readLisp rest
  where ([c]:rest) = chunkLispTree s
