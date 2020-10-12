nat :: [Integer]
nat = asc 1
  where asc n = n : asc (n + 1)

nat2 :: [Integer]
nat2 = 1 : map (+1) nat2

fibo :: [Integer]
fibo = 0 : 1 : calc 0 1
  where calc n m = n + m : calc m (n + m)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


primes :: [Integer]
-- primes = 2 : (init primes)
-- primes = [p | p <- [2..], p' <- primes,  p `div` p' /= 0]
-- primes = 2 : 3 : minus
-- primes = 2 : filter (\x -> null [p | p <- init primes, x `mod` p /= 0]) primes
primes = seive [2..]
  where seive (p:xs) = p : [x | x <- seive xs, x `mod` p /= 0]
