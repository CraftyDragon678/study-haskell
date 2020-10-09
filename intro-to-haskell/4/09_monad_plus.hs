-- 방금 전 만든 failure, (+++)
-- 항상 실패([]) 하는 파서, 성공할 때까지 오른쪽으로 가는 파서이다.

-- MonadPlus는 이런 두가지 특징을 구현한 모나드다.
-- failure -> `mzero`, (+++) -> `mplus`

import Control.Monad
import GHC.Base

infixr 5 +++


newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
    return v = P $ \inp -> [(v, inp)]
    p >>= f = P $ \inp -> case parse p inp of
        [] -> []
        [(v, out)] -> parse (f v) out
instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Parser where
    mzero = P $ \_ -> []
    p `mplus` q = P $ \inp -> case parse p inp of
        [] -> parse q inp
        [(v, out)] -> [(v, out)]

item :: Parser Char
item = P $ \xs -> case xs of
  [] -> []
  (x:xs) -> [(x, xs)]

failure :: Parser Char
failure = mzero

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q
