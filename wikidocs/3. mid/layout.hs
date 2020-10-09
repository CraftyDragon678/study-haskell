doGuessing num = do
  putStr "Enter your guess: "
  guess <- getLine
  case compare (read guess) num of
    LT -> do putStrLn "Too low!"
             doGuessing num
    GT -> do putStrLn "Too high!"
             doGuessing num
    EQ -> putStrLn "You Win!"

doGuessing2 num = do {
  putStr "Enter your guess: ";
  guess <- getLine;
  case compare (read guess) num of {
    LT -> do {
        putStrLn "Too low!";
        doGuessing num
      };
    GT -> do {
        putStrLn "Too high!";
        doGuessing num
      };
    EQ -> putStrLn "You Win!"
  }
}


main = do
  doGuessing 30