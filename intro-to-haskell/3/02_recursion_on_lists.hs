product1 :: [Int] -> Int
product1 [] = 1
product1 (n : ns) = n * productC ns

length1 :: [a] -> Int
length1 [] = 0
length1 (x : xs) = 1 + length1 xs

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x : xs) = reverse1 xs ++ [x]

zip1 :: [a] -> [b] -> [(a, b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (x:xs) (y:ys) = (x, y) : zip xs ys  -- cons

drop1 :: Int -> [a] -> [a]
drop1 0 xs = xs
drop1 _ [] = []
drop1 n (x:xs) = drop1 (n-1) xs

(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)
