import Test.QuickCheck
import Data.List hiding (insert)

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node l v r)
  | x < v = Node (insert x l) v r
  | x > v = Node l v (insert x r)
  | x == v = error "can't be same"

inorder :: (Show a) => Tree a -> [a]
inorder Leaf = []
inorder (Node l v r) = inorder l ++ [v] ++ inorder r

mockTree :: Tree Integer
mockTree = Node
  (Node
    (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf))
  4
  (Node
    Leaf
    5 
    (Node
      Leaf
      6
      Leaf))

-- prop :: (Ord a, Show a) => a -> Tree a -> Bool
-- prop x t = (isSorted $ inorder $ insert x t) == True
--   where
--     isSorted [] = True
    -- isSorted (x:xs) = (x < head xs) && isSorted xs

prop xs = rmdup xs == xs ==> sort xs === xs'
  where
    types = xs :: [Int]
    xs' = inorder $ foldr insert Leaf xs
    rmdup [] = []
    rmdup [x] = [x]
    rmdup (x:xs) = x : filter (/=x) (rmdup xs)
