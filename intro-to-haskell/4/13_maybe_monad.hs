import Prelude hiding (Maybe, Just, Nothing)

data Maybe a = Nothing | Just a

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  pure = Just
  Just f <*> m = fmap f m
  Nothing <*> _m = Nothing

instance Monad Maybe where
  -- return x = Just x
  Nothing >>= _ = Nothing