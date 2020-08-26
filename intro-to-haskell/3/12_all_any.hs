-- list comprehension: list만
-- foldr: 모든 순회 가능한 데이터 타입

-- 모든 원소가 조건을 만족?
all :: (a -> Bool) -> [a] -> Bool
-- all f xs = and [f x | x <- xs]
all f xs = foldr (\x acc -> f x && acc) True xs

-- 한 원소라도 조건을 만족?
any :: (a -> Bool) -> [a] -> Bool
-- any f xs = or [f x | x <- xs]
any f xs = foldr (\x acc -> f x || acc) False xs
