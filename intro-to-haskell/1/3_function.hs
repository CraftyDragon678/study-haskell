double x = x + x
quadruple x = double (double x)
-- 함수의 우선 순위가 다른 것보다 높아서 double double x로 적으면, double(double)(x)가 됨

factorial n = product [1..n]
avg ns = sum ns `div` genericLength ns
-- ns(정수 배열) 합을 길이로 나눈다.
-- avg ns = div (sum ns) (genericLength ns)
