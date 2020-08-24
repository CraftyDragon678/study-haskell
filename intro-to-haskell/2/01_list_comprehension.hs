concat1 :: [[a]] -> [a]
concat1 xss = [x | xs <- xss, x <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

-- zip :: [a] -> [b] -> [(a, b)]
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                  where n = (length xs) - 1

main = do
  putStrLn "리스트 내포 (list comprehension)"

  putStrLn "x <- [1..5] 를 generator라고 부릅니다."
  print [x^2 | x <- [1..3]]

  putStrLn "조합을 만들 수도 있다."
  print [(x, y) | x <- [1..3], y <- [3..5]]

  putStrLn "변수를 제너레이터 안에 (dependant generator)"
  print [(x, y) | x <- [1..3], y <- [x+2]]

  putStrLn "concat을 dependant generator를 활용해서"
  print (concat1 [[1, 3], [42, 5], [3]])

  putStrLn "guards를 사용해서 변수를 걸러낼 수 있음"
  print [x | x <- [1..10], x `mod` 2 == 0]
  print [x | x <- [1..10], even x]

  putStrLn "약수"
  print (factors 10)

  putStrLn "소수"
  print (prime 17)
  print (prime 10)

  print (primes 40)

  putStrLn "정렬이 되었는지 검사"
  print (sorted [1, 2, 6, 9])
  print (sorted [3, 1, 2, 6, 9])

  putStrLn "finds"
  print (positions 0 [0, 3, 1, 0, 5, 3, 0])


