-- fold right를 이용한 정리
-- 초기값(v)를 바탕으로 f를 적용해나감
-- 콘싱과 매우매우매우매우 비슷하다.
foldrC :: (a -> b -> b) -> b -> [a] -> b
foldrC f v [] = v
foldrC f v (x:xs) = f x (foldr f v xs)

sumC = foldrC (+) 0
productC = foldrC (*) 1
orC = foldrC (||) False
andC = foldrC (&&) True
lengthC xs = foldrC (\_ n -> 1 + n) 0
reverseC xs = foldrC (\x xs -> xs ++ [x]) []
filterC f xs = foldrC (\x acc -> if f x then x : acc else acc) [] xs
mapC f xs = foldrC (\x acc -> f x : acc) [] xs

