sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [x'|x' <- xs, x' <= x] ++ [x] ++ sort [x'|x' <- xs, x' > x]
