import Prelude hiding (elem)

-- Eq is a type class
elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) = if x == y then True else elem x ys

-- elem e (x:xs) = (e == x) || elem e xs
-- wow...

nub :: (Eq a) => [a] -> [a]
nub [] = []
-- nub (x:xs) = [x | not $ elem x xs] ++ nub xs

-- nub (x:xs)
--   | x `elem` xs = nub xs
--   | otherwise = x : nub xs

-- nub (x:xs) = filter (/= x) (nub xs)

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [_] = True
isAsc (x:xs) = (x <= head xs) && isAsc xs

-- isAsc (x:y:xs) = (x <= y) && isAsc (y:xs)

hasPath :: [(Int,Int)] -> Int -> Int -> Bool
-- hasPath [] _ _ = False
-- hasPath [(x,y)] a b = (x == a && y == b)
-- hasPath (x:xs) a b = 

hasPath [] x y = x == y
hasPath xs x y
  | x == y = True
  | otherwise = 
    let xs' = [(n,m) | (n,m) <- xs, n /= x] in
      or [hasPath xs' m y | (n,m) <- xs, n == x]
