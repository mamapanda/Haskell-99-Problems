module Q55To60 where

import Data.Ix

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Eq)

instance Show a => Show (Tree a) where
  show = showTree "" 'c'
    where showTree _ _ Empty = ""
          showTree padding sideChar (Branch x lhs rhs) =
            let line = padding ++ label sideChar
                padding' =  map (const ' ') line
            in line ++ show x ++ "\n"
               ++ showTree padding' 'l' lhs
               ++ showTree padding' 'r' rhs
          label sideChar = sideChar:"|- "

left::Tree a -> Tree a
left Empty = error "An Empty tree node does not have any subtrees."
left (Branch _ lhs _) = lhs

right::Tree a -> Tree a
right Empty = error "An Empty tree node does not have any subtrees."
right (Branch _ _ rhs) = rhs

rootLabel::Tree a -> a
rootLabel Empty = error "An Empty tree node does not have a root label."
rootLabel (Branch x _ _) = x

-- | Question 55
cbalTree::Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = [ Branch 'x' l r
             | let subcount = n - 1
                   n' = quot subcount 2
             , (lcount, rcount) <- if odd subcount
                                   then [(n', n' + 1), (n' + 1, n')]
                                   else [(n', n')]
             , l <- cbalTree lcount
             , r <- cbalTree rcount
             ]

-- | Question 56
symmetric::Tree a -> Bool
symmetric Empty = True
symmetric tree = mirror (left tree) (right tree)
  where mirror Empty Empty = True
        mirror (Branch _ l r) (Branch _ l' r') = mirror l r' && mirror r l'
        mirror _ _ = False

-- | Question 57
construct::[Int] -> Tree Int
construct = foldl insertNode Empty
  where insertNode Empty x = Branch x Empty Empty
        insertNode (Branch label lhs rhs) x
          | x < label = Branch label (insertNode lhs x) rhs
          | otherwise = Branch label lhs (insertNode rhs x)

-- | Question 58
symCbalTrees::Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree

-- | Question 59
hbalTree::Show a => a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree val 1 = [Branch val Empty Empty]
hbalTree val h = [ Branch val l r
                 | let h' = h - 1
                 , (lh, rh) <- [(h', h'), (h', h' - 1), (h' - 1, h')]
                 , l <- hbalTree val lh
                 , r <- hbalTree val rh
                 ]

-- | Question 60
minNodesList::[Int]
minNodesList = 0:1:(zipWith (\a b -> a + b + 1) <*> tail) minNodesList

minNodes::Int -> Int
minNodes = (minNodesList!!)

maxNodes::Int -> Int
maxNodes h = 2 ^ h - 1

minHeight::Int -> Int
minHeight = (ceiling::Double -> Int) . logBase 2 . fromIntegral

maxHeight::Int -> Int
maxHeight n = snd . head . dropWhile ((< n) . fst) $ zip minNodesList [0..]

hbalTreeNodes::Show a => a -> Int -> [Tree a]
hbalTreeNodes val n = concatMap (hbalTreeNodes' n) [(minHeight n)..(maxHeight n)]
  where hbalTreeNodes' 0 0 = [Empty]
        hbalTreeNodes' _ 0 = error "Q60"
        hbalTreeNodes' 1 1 = [Branch val Empty Empty]
        hbalTreeNodes' _ 1 = error "Q60"
        hbalTreeNodes' n' h = [ Branch val lBranch rBranch
                              | let h' = h - 1
                              , (lh, rh) <- [(h', h'), (h', h' - 1), (h' - 1, h')]
                              , lNodes <- [(minNodes lh)..(maxNodes lh)]
                              , let rNodes = n' - lNodes - 1
                              , inRange (minNodes rh, maxNodes rh) rNodes
                              , lBranch <- hbalTreeNodes' lNodes lh
                              , rBranch <- hbalTreeNodes' rNodes rh
                              ]
