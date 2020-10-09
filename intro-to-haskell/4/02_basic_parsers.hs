type Parser a = String -> [(a, String)]

item :: Parser Char   -- 문자열의 첫 부분을 파싱하는 파서
item = \xs -> case xs of
  [] -> []            -- 실패했당,,, 문자열이 아무것도 없어서
  (x:xs) -> [(x, xs)]

-- 항상 []만 => 항상 실패하는 파서
failure :: Parser a
failure = \_ -> []

returnC :: a -> Parser a  -- 항상 성공하는 파서
returnC v = \xs -> [(v, xs)]

-- 두개의 파서를 합치는 함수 (+++)
-- p가 성공하면 p의 리턴값, 실패하면 q가 처리
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \xs -> case p xs of
  [] -> q xs
  [(y, ys)] -> [(y, ys)]

