not :: Bool -> Bool
not False = True
not True = False

-- switch~case없이 나타낼 수 있음

(&&&) :: Bool -> Bool -> Bool

True &&& True = True
True &&& False = False
False &&& True = False
False &&& False = False

-- same as

True &&& True = True -- 오른쪽이 True인지 평가(evaluate)함
_ &&& _ = False  -- wild card pattern

-- same as

True &&& b = b -- 오른쪽을 평가하지 않음
False &&& _ = False

-- b && b = b -- error

False ||| False = False
_ ||| _ = True
