-- 원본 타입 String을 가지는 같은 종류의 파서는
-- 계~~~속 연결할 수 있다.

-- p1, ..., pn을 파서, v1, ..., vn을 부가정보라고 하면

{- 
m = p1 >>= \v1 ->
  p2 >>= \v2 ->
  p3 >>= \v3 ->
  ...
  pn >>= \vn ->
  return (f v1 v2 ... vn)
 -}
-- 이렇게 쓸 수 있는데, do 문법을 사용하면 아래와 같이 쓸 수도 있다.
{- 
do v1 <- p1
   v2 <- p2
   ...
   vn <- pn
   return (f v1 v2 ... vn)
 -}
