import Data.List (isInfixOf)
-- import Data.Char (isLetter, isSpace)

main :: IO ()
main = do
  putStrLn "Specify the words to search"
  xs <- getWords
  putStr "File to search: "
  filePath <- getLine
  file <- readFile filePath
  findStrings xs file

getWords :: IO [String]
getWords = getWord
  where
    getWord :: IO [String]
    getWord = do
      putStr "> "
      x <- getLine
      if x == "" then return []
      else do
        xs <- getWord
        return $ x:xs

findStrings :: [String] -> String -> IO ()
findStrings [] _ = return ()
findStrings (x:xs) a = do
  putStr $ "\"" ++ x ++ "\" "
  if x `isInfixOf` a then putStrLn "found" else putStrLn "NOT found"
  findStrings xs a

-- findStrings2 :: [String] -> String -> [String]
-- findStrings2 sws text = [w | w <- sws, w `elem` txtwords]
--   where
--     ftext = filter (\x -> isLetter x || isSpace x) text
--     txtwords = words ftext