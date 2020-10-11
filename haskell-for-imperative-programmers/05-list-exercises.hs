import Prelude hiding (elem)

elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) = if x == y then True else elem x ys

-- elem e (x:xs) = (e == x) || elem e xs
-- wow...
