-- \를 이용해 익명함수를 표현
-- currying을 좀더 의미있게 표현하거나 네이밍의 지옥에서 벗어날 수 있다.


-- add x y = x + y

-- same as

add = \x -> (\y -> x + y)

-- const1 x _ = x
const1 = \a -> (\b -> a)

odds n = filter f [0..n]
  where
    f x = x `mod` 2 /= 0

odds1 n = filter (\x -> x `mod` 2 /= 0) [0..n]
