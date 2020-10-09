-- 원래처럼 함수를 사용해서 정의를 한다면,,,
zero f x = x
one f x = f x
two f x = (f . f) x

-- 람다를 활용한 아래와 같음
zero = \s z -> z
one = \s z -> s z
two = \s z -> s (s z)

-- two = \s z -> (s . s) z
-- two = \s -> s . s

c2i x = x (+1) 0
c2s x = x ('*':) ""

-- c2i zero = 0
-- c2i one = 1
-- c2i two = 2
-- c2s zero = ""
-- c2s one = "*"
-- c2s two = "**"

-- 증명 part~~~ 문법과는 맞지 않아요~~
-- x 와 y 함수를 합치고 싶다,,
-- 결과값을 각각 x', y'라고 하면
x' = c2i x
y' = c2i y

-- 어쨌는 위 두개는 수
-- 두개를 더할 거임 ㅇㅇ

-- 이런 꼴이라고 가정
x' + y' = c2i (add x y)

two = \s -> s . s
two = \s z -> (s . s) z

c2i x = x (+1) 0
-- 합 증명 타임~
x' + y'
= c2i x + c2i y
= x (+1) 0 + c2i y
= x (+1) (c2i y)
= x (+1) (y (+1) 0)
= (\s z -> x s (y s z)) (+1) 0  -- 이 부분
-- z: init value
-- s: successor function -> 공통인 친구니깐 얘를 찢어서 같이 둠
-- y s z의 값을 x s 의 인자로 전달
-- y s z에서 s z는 (+1) 와 0


-- add 함수의 역할은 아래와 같습니다
x' + y' = (add x y) (+1) 0
= c2i (add x y)

add x y = (\s z -> x s (y s z))

c2i $ add one two


-- 곱셈,,
two = \s -> s.s
three = \s -> s.s.s
four = \s -> s.s.s.s
five = \s -> s.s.s.s.s

mul = \s z -> s (y s) z
c2i (mul two five)


-- 정리
-- 더하는 연산은 초기값이 이전 함수의 결과가 되고,
-- 곱하는 연산은 successor 함수가 이전 함수에 successor를 넣은 함수가 된다

-- identity
id :: a -> a
id = \x -> x

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

-- 함수의 합성
compose [(+1), (*4)] 0  -- 1
compose [(+1), (*4)] 5  -- 21
compose [(*4), (+1)] 0  -- 4

