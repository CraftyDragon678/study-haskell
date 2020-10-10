import System.IO (hSetEcho, stdin)

hangman :: IO ()
hangman = do putStr "Think of a word: "
             word <- sgetLine
             putStr "Try to guess it: "
             guess word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n'
                then do putChar x
                        return []
                else do putChar '-'
                        xs <- sgetLine
                        return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

guess :: String -> IO ()
guess word = do putStr "> "
                xs <- getLine
                if xs == word
                  then putStrLn "You got it!"
                  else do putStrLn (diff word xs)
                          guess word

diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '-' | x <- xs]
