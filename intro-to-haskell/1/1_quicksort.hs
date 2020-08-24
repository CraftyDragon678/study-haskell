f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
  where
    ys = [a | a <- xs, a <= x]
    zs = [b | b <- xs, b > x]

{-
ghci를 열고 `:l quicksort.hs` 입력
f [5, 3, 2, 6]
[2,3,5,6]
-}
