import System.IO (hSetEcho, stdin)
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

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      `mplus` nat

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token $ string xs

integer :: Parser Int
integer = token int

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           `mplus` do symbol "-"
                      e <- expr
                      return (t - e)
           `mplus` return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (t * f)
           `mplus` do symbol "/"
                      t <- term
                      return (t `div` f)
           `mplus` return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         `mplus` natural

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

beep :: IO ()
beep = putChar '\BEL'

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt :: Pos -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs

box :: [String]
box =  ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",  -- quit, clear, delete evaluation
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x:xs) = do x
                 seqn xs

buttons :: [Char]
buttons = standard ++ extra
          where
            standard = "qcd=123+456-789*0()/"
            extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = do cls
             seqn [writeAt (1, y) line | (y, line) <- zip [1..13] box]

display :: String -> IO ()
display xs = do writeAt (3, 2) "             "
                writeAt (3, 2) $ reverse $ take 13 $ reverse xs

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons
               then process c xs
               else do beep
                       calc xs

process :: Char -> String -> IO ()
process c xs
  | elem c "qQ\ESC" = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "=\n" = eval xs
  | elem c "cC" = clear
  | otherwise = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
            [(n, "")] -> calc (show n)
            _ -> do beep
                    calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear
