-- 문자열은 캐릭터의 리스트
import Data.Char

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]
