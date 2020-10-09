-- https://www.seas.upenn.edu/~cis552/15fa/lectures/Parsers.html
import Control.Monad

-- a는 타입매개변수 
newtype Parser a = P (String -> [(a, String)])

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
  
item :: Parser Char
item = P $ \inp -> case inp of
  [] -> []
  (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

ignore2 :: Parser (Char, Char)
ignore2 = do
  x <- item
  item
  z <- item
  return (x, z)
  
