# 모나드 공리

하스켈에서 Monad 타입 클래스의 모든 인스턴스와
return, (>>=)의 구현은 다음을 모두 지켜야 한다.

## right unit - 우단위원의 법칙
m >>= return == m

## left unit - 좌단위원의 법칙
return x >>= f == f x

## associativity - 결합법칙
(m >>= f) >>= g == m >>= (\x -> f x >>= g)

