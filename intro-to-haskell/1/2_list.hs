main = do
  putStrLn "리스트는 한 타입만 가질 수 있습니다."
  putStr "head [2, 3, 4] \t == "
  print (head [2, 3, 4])

  putStr "tail [2, 3, 4] \t == "
  print (tail [2, 3, 4])

  putStr "head [2] \t == "
  print (head [2])

  putStr "tail [2] \t == "
  print (tail [2])

  putStr "init [2, 3, 4] \t == "
  print (init [2, 3, 4])

  putStr "last [2, 3, 4] \t == "
  print (last [2, 3, 4])

  putStr "take 2 [2, 3, 4] == "
  print (take 2 [2, 3, 4])

  putStr "drop 2 [2, 3, 4] == "
  print (drop 2 [2, 3, 4])

  putStrLn "\n리스트 원소 접근: !!"

  putStr "[1, 2, 3, 4] !! 0 == "
  print ([1, 2, 3, 4] !! 0)

  putStrLn "\n리스트 길이: length"

  putStr "length [1, 2, 3, 4] == "
  print (length [1, 2, 3, 4])

  putStrLn "\nproduct, ++, reverse"
  
  putStr "product [1..5] == "
  print (product [1..5])

  putStr "[2, 3] ++ [5] ++ [2] == "
  print ([2, 3] ++ [5] ++ [2])

  putStr "reverse [12, 3, 5, 4] == "
  print (reverse [12, 3, 5, 4])
