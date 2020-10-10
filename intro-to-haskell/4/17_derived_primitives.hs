import Prelude hiding (sequence)

-- 모나드에 대한 map
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = do x <- mx
                return (f x)
             
-- 모나드에 대한 concat
join :: Monad m => m (m a) -> m a
join mmx = do mx <- mmx
              x <- mx
              return x

--  
(>>) :: Monad m => m a -> m b -> m b
mx >> my = do _ <- mx
              y <- my
              return y
              
sequence :: Monad m => [m a] -> m [a]
sequence (mx:mxs) = do x <- mx
                       xs <- sequence mxs
                       return (x:xs)