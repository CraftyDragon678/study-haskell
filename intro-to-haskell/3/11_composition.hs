-- 함수 합성
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x f(g x)

odd :: Int -> Bool
odd :: not . even

twice f = f . f

