main = do
  putStrLn "불리언은 대소문자를 구분합니다."
  print False
  print True

  putStrLn "\nfst snd는 2-tuple에서 첫번째, 두번째 값을 돌려줍니다."

  putStr "fst (1, \"Hello\") == "
  print (fst (1, "Hello"))

  putStr "snd (1, \"Hello\") == "
  print (snd (1, "Hello"))
