import Data.List

f :: [[Int]]
f = iterate (concatMap (\x -> [head x, length x]) . group) [1]

f2 :: [Int] -> [Int]
f2 xs = concatMap (\(x,y) -> [x, y]) [(head x, length x) | x <- group xs]

f3 :: [[Int]]
f3 = iterate (\xs -> concatMap (\(x,y) -> [x, y]) [(head x, length x) | x <- group xs]) [1]


main :: IO ()
main = do
  putStr "> "
  x <- getLine
  let n = read x :: Int

  putStrLn $ intercalate "\n" $ map (intercalate " " . map show) $ take n f
