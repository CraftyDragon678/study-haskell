import Data.List (isInfixOf)

main :: IO ()
main = do
  putStrLn "Specify the words to search"
  xs <- getWords
  putStr "File to search: "
  filePath <- getLine
  file <- readFile filePath
  -- putStrLn $ unlines xs
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
