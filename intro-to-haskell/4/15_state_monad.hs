import GHC.Base (ap, liftM)

-- 한 상태에서 다른 상태로 변환시켜주는 state transformer
-- parser
type State = Int
newtype ST a = S (State -> (a, State))

-- dummy constructor를 제거하기 위해
apply :: ST a -> State -> (a, State)
apply (S f) x = f x

instance Functor ST where
  fmap = liftM
instance Applicative ST where
  pure = return
  (<*>) = ap
instance Monad ST where
  return x = S $ \s -> (x, s)
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  -- (>>=) :: (State -> (a, State)) -> (a -> State -> (b, State)) -> (State -> (b, State))
  -- f :: (a -> ST b)
  st >>= f = S $ \s -> let (x, s') = apply st s -- x: a, s': State
                       in apply (f x) s'

-- >>= 는 모나드(연산)간 연결

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)
-- instance (Show t) => Show (Tree t) where
--   show (Leaf a) = show a
--   show (Node l r) = "Node (" ++ show l ++ ", " ++ show r ++ ")"

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- 이걸 이용해서 tree에 번호를 붙일 수 있음
fresh :: ST Int
fresh = S (\n -> (n, n + 1))

mlabel :: Tree a -> ST (Tree (a, Int))
mlabel (Leaf x) = do n <- fresh
                     return (Leaf (x, n))
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')

label :: Tree a -> Tree (a, Int)
label t = fst (apply (mlabel t) 0)
