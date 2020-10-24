import Data.List

f :: [[Int]]
f = iterate (concatMap (\x -> [head x, length x]) . group) [1]

main :: IO ()
main = do
  putStr "> "
  x <- getLine
  let n = read x :: Int

  putStrLn $ intercalate "\n" $ map (intercalate " " . map show) $ take n f
