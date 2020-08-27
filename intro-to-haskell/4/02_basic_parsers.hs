type Parser a = String -> [(a, String)]

item :: Parser Char
item = \xs -> case xs of
  [] -> []
  (x:xs) -> [(x, xs)]

-- 항상 []만
failure :: Parser a
failure = \xs -> []

returnC :: a -> Parser a
returnC v = \xs -> [(v, xs)]

-- 두개의 파서를 합치는 함수 (+++)
-- p가 성공하면 p의 리턴값, 실패하면 q가 처리
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \xs -> case p xs of
  [] -> q xs
  [(y, ys)] -> [(y, ys)]

