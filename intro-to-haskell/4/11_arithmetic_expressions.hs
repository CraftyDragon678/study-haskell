import GHC.Base
    ( Applicative(liftA2),
      MonadPlus(..),
      Alternative(many, (<|>), empty),
      ap,
      liftM )
import Data.Char
    ( isAlphaNum, isDigit, isAlpha, isLower, isSpace, isUpper )

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
item = P $ \xs ->
  case xs of
    [] -> []
    (x:xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else mzero

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- many digit 이라는 파서를 사용하면 여러 문자의 숫자를 파싱할 수 있다.

-- many1는 최소한 1번은 파싱을 하는 함수이다.
many1 :: MonadPlus f  => f a -> f [a]
many1 p = liftA2 (:) p (many p)
-- many1 p = do x <- p
--              xs <- many p
--              return (x:xs)

-- 단어 하나를 가져옴
ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- many1 digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

-- 파서를 받아서 앞 뒤로 붙은 스페이스를 제거하는 기능이 있는 파서를 돌려주는 함수
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token $ string xs

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
             `mplus` return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (t * f)
             `mplus` return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         `mplus` natural

eval :: String -> Int
eval xs = case parse expr xs of
  [(x, "")] -> x
  [] -> error ("invalid input" ++ xs)
  [(_, out)] -> error ("unused output" ++ out)

