data Expr = Val Int | Div Expr Expr | Op Expr Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

-- eval (Val 3)
-- 3

-- eval (Div (Val 3) (Val 4))
-- 0

-- eval (Div (Val 8) (Val 4))
-- 2

-- eval (Div (Val 8) (Val 0))
-- *** Exception: divide by zero => 해결 해주기

-- 어쩌면 정수
safediv :: Int -> Int -> Maybe Int
safediv n m = if m == 0 then Nothing else Just $ n `div` m

eval2 :: Expr -> Maybe Int
eval2 (Val n) = Just n
eval2 (Div x y) = case eval2 x of
                   Nothing -> Nothing
                   Just n -> case eval2 y of
                              Nothing -> Nothing
                              Just m -> safediv n m

seqn :: Maybe a -> Maybe b -> Maybe (a, b)
seqn _ Nothing = Nothing
seqn Nothing _ = Nothing
seqn (Just x) (Just y) = Just (x, y)

apply :: (a -> Maybe b) -> Maybe a -> Maybe b
apply _ Nothing = Nothing
apply f (Just x) = f x

eval3 :: Expr -> Maybe Int
eval3 (Val n) = Just n
eval3 (Div x y) = apply f (eval3 x `seqn` eval3 y)
                  where f (n, m) = safediv n m

-- safeadd :: Int -> 

-- 만약에.. 인자를 3개 받는다면
eval3 (Op x y z) = apply f (eval3 x `seqn` eval3 y `seqn` eval3 z)
                    where f ((a, b), c) = Just $ a + b + c
-- 이렇게도 짤 수 있지만, 괄호가 계속 중첩 됨

-- bind연산자 사용해서 추상화

{- 
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
m >>= f = case m of
            Nothing -> Nothing
            Just x -> f x
-}
-- 있으면 계속 적용하고 없으면 멈추는 함수

eval4 :: Expr -> Maybe Int
eval4 (Val n) = Just n
eval4 (Div x y) = eval4 x >>= \n ->
                  eval4 y >>= \m ->
                  safediv n m

-- do를 쓰면 이렇게 쓸 수 있겠져?
eval5 :: Expr -> Maybe Int
eval5 (Val n) = Just n
eval5 (Div x y) = do n <- eval5 x
                     m <- eval5 y
                     safediv n m
