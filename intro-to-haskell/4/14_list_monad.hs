-- instance Monad [] where
--   return x = [x]
--   xs >>= f = concat (map f xs)

-- xs >>= f = [y | x <- xs, y <- f x]

pairs :: [a] -> [b] -> [(a, b)]
-- pairs xs ys = xs >>= \x -> ys >>= \y -> return (x, y)
pairs xs ys = do x <- xs
                 y <- ys
                 return (x, y)