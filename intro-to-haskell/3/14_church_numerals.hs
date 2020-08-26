zero = \s z -> z
one = \s z -> s z
two = \s z -> s (s z)

-- two = \s z -> (s . s) z
-- two = \s -> s . s

c2i x = x (+1) 0
c2s x = x ('*':) ""


-- 증명 part~~~ 문법과는 맞지 않아요~~
-- x 와 y 함수를 합치고 싶다,,
-- 결과값을 각각 x', y'라고 하면
x' = c2i x
y' = c2i y

-- 어쨌는 위 두개는 수
-- 두개를 더할 거임 ㅇㅇ

-- 증명 타임~
x' + y'
= c2i x + c2i y
= x (+1) 0 + c2i y
= x (+1) (c2i y)
= x (+1) (y (+1) 0)
= (\s z -> x s (y s z)) (+1) 0

-- add 함수의 역할은 아래와 같습니다
x' + y' = (add x y) (+1) 0
= c2i (add x y)

add x y = (\s z -> x s (y s z))

c2i (add one two)


-- 곱셈,,
two = \s -> s . s
three = \s -> s . s . s

mul = \s z -> s (y s) z
c2i (mul two five)

id :: a -> a
id = \x -> x

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

