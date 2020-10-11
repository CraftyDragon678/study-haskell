rev :: [a] -> [a]
-- rev [] = []
-- rev [x] = [x]
-- rev (x:xs) =  rev xs ++ [x]

-- rev = foldr (\x y -> y ++ [x]) []
rev = foldl (\x y -> y : x) []

rev2 :: [a] -> [a]
rev2 = foldl (flip (:)) []

prefixes :: [a] -> [[a]]
-- prefixes [] = []
-- prefixes (x:xs) = foldl (\x y -> x ++ [(last x) ++ [y]]) [[x]] xs

prefixes = foldr (\x acc -> [x] : map (x:) acc) []

lagrange :: [(Float,Float)] -> Float -> Float
-- lagrange xs x = foldl (\acc v -> acc * ((x - fst v) / ())) xs

-- lagrange xs x = foldr (\v acc -> snd v + acc) 0 xs
-- lagrange xs x = foldr ((+) . snd) 0 xs

-- lagrange xs x = foldr (\v -> (snd v * foldr (\_ _ -> 1) 1 xs +)) 0 xs

-- no
-- lagrange xs x = foldr (\v ->
--     (snd v *
--       foldr (\(x', _) ->((x - x') / (fst v - x') *)) 1 [(x',y') | (x',y') <- xs, x /= x']
--     +)
--   ) 0 xs

-- lagrange xs x = foldr (\v ->
--     (snd v *
--       foldr (\(x', _) ->((x - x') / (fst v - x') *)) 1 [(x',y') | (x',y') <- xs, fst v /= x']
--     +)
--   ) 0 xs


-- 중복되는 xj는 없다
lagrange xs x = foldr (\(xj,yj) -> (yj * l xj +)) 0 xs
  where
    l xj = foldr (\xm -> ((x - xm) / (xj - xm) *)) 1 arr
      where
        arr = [xm | (xm,_) <- xs, xm /= xj]

lagrange2 :: [(Float, Float)] -> Float -> Float
{- 
lagrange2 xs x = foldl (\acc (xj,y) -> acc + (y * l xj)) 0 xs
  where 
    l xj = foldl (
      \acc (xk,_) -> 
        if xj == xk
          then acc
          else acc * ((x-xk) / (xj-xk))
      ) 1 xs
 -}
lagrange2 xs x = foldr (\(xj,y) -> (y * l xj +)) 0 xs
  where 
    l xj = foldl (
      \acc (xk,_) -> 
        if xj == xk
          then acc
          else acc * ((x-xk) / (xj-xk))
      ) 1 xs


data Tree a = Leaf a | Node a [Tree a]

t :: Tree Char
t = Node 'c' [
      Node 'a'
        [Leaf 'r', Leaf 't'],
      Node 'o'
        [Node 'o'
          [Leaf 'l']]]

-- showTree :: (Show a) => Tree a -> String
-- showTree t = 

foldTree :: (b -> a -> b) -> b -> Tree a -> b
foldTree f acc (Leaf x) = f acc x
foldTree f acc (Node x xs) = foldl (foldTree f) (f acc x) xs
