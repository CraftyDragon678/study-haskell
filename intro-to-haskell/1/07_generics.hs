length :: [a] -> Int
-- 여기서 a는 어떤 타입이던지 상관이 없다.
-- a를 type variable라고 부르고, 하스켈에선 소문자로 쓴다.


sum :: (Foldable t, Num a) => t a -> a
-- 여기서 a는 타입이 제한 된다.

palindrome xs = reverse xs == xs
-- palindrome :: Eq [a] => [a] -> Bool
-- 비교 가능한 Eq 타입이어야 함을 알려줌
