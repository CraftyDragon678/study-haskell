main :: IO ()
main = do
  putStr "Write String: "
  x <- getLine
  if x /= "quit" then do
    putStrLn ("Input " ++ x)
    main
  else
    return ()