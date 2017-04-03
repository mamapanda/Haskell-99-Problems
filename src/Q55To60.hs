module Q55To60 where

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Eq)

instance Show a => Show (Tree a) where
  show = showTree label
    where showTree _ Empty = ""
          showTree line (Branch x lhs rhs) =
            let line' = map (const ' ') line ++ label
            in line ++ show x ++ "\n"
               ++ showTree line' lhs
               ++ showTree line' rhs
          label = "|- "

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
cbalTree::Int -> Tree Char
cbalTree 0 = Empty
cbalTree 1 = Branch 'x' Empty Empty
cbalTree n = Branch 'x' (cbalTree (quot (n' + 1) 2)) (cbalTree (quot n' 2))
  where n' = n - 1

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
