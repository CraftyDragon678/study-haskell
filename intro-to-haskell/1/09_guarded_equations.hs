{-
abs n = if n >= 0 then n else -n
signnum n = if n > 0 then 1 else
              if n < 0 then -1 else 0
-}

-- same as

abs n | n >= 0 = n
      | otherwise = -n

signnum n | n > 0 = 1
          | n < 0 = -1
          | otherwise = 0

-- otherwise는 True라고 선언되어있음
