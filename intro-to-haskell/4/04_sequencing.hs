-- 아니,,, 저번에 만든 (+++) 이건 하나의 파서는 사용되지 않고 죽잖아
-- 그러니깐 둘다 엮어서 하나의 파서를 만들고 싶다 이 말이야
type Parser a = String -> [(a, String)]

-- 근데 그냥 Parser a 의 출력을 Parser b로 넘겨 주고자 하니
-- Parser a 의 출력이 String이 아니고 [(a, String)] 인거야
-- 근데 근데 튜플의 두번째 String만 넘겨 주기도 좀 그래
-- 부가 정보 a도 함께 넘어가야 의미가 있다는거지

-- 그래서 중간 함수를 만들어서 넣어줄 거임
-- Parser a 에서 a가 무엇인지에 따라
-- 중간 함수의 형태가 바뀌어야 하니깐 
-- 아래에서 p가 Parser a, q가 Parser b로 바꾸는 중간 함수이자 Parser b

-- 모나드의 bind(>>=)를 정의함
(>>==) :: Parser a -> (a -> Parser b) -> Parser b
p >>== q = \xs -> case p xs of
  [] -> []                       -- p가 실패하면 아무것도 안 함
  [(y, ys)] -> (q y) ys          -- p가 성공하면 그 y를 q에 다시 넘겨서 파서 b를 만들고, 거기에 남은 문자열을 넘겨줌

-- 실제로 bind의 타입은 `Monad m => m a -> (a -> m b) -> m b` 이다.


returnC :: a -> Parser a
returnC v = \xs -> [(v, xs)]

parseTwice :: Parser (Char, Char)
parseTwice = item >>== \x -> returnC (x, x)

item :: Parser Char
item = \xs -> case xs of
 [] -> []
 (x:xs) -> [(x, xs)]

ignore2 :: Parser (Char, Char)
ignore2 = item >>== \x -> item >>== \y -> item >>== \z -> return (x, z)

