# Basic Classes
Eq: 해당 클래스가 비교할 수 있음 (==, /=)

Ord: 순서가 있음 (>, >=, <, <=, max, min)

Bool, Char, String, Int, Integer, Float, Tuple, List는 모두
Ord와 Eq클래스의 인스턴스이다.


# Showable Type
Show클래스의 인스턴스는 show함수로 toString과 같은 효과를 냄

# Readable Type
read함수를 이용해 String으로부터 얻을 수 있음 (parse)

# Integral, Fractional, Rational
숫자 클래스가 Num만 있는 것은 아님.

정수, 분수, 유리수에서 적용 가능한  함수가 있다.
mod나 div는 Integral에만, (/)는 Fractional에만 적용 가능

