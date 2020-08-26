twice :: (a -> a) -> a -> a
twice f x = f (f x)

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

-- or

map f [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> a -> a
filter f xs = [x|x <- xs, f x]

-- or

filter f [] = []
filter f (x:xs)
  | f x = x : filter x xs
  | otherwise = filter x xs

sum [] = 0
sum (x:xs) = x + sum xs

product [] = 1
product (x:xs) = x * product xs

and [] = True
and (x:xs) = x && and xs

-- 이것은 마치,,,
f [] = v -- v: 초기값
f (x:xs) = x pred f xs  -- pred: 특정 연산
