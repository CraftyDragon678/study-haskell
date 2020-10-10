import Prelude hiding (getLine)

-- return  :: a -> IO a
-- (>>=)   :: IO a -> (a -> IO b) -> IO b
-- getChar :: IO Char
-- putChar :: Char -> IO ()

getLine :: IO String
getLine = do x <- getChar
             if x == '\n'
               then return []
               else do xs <- getLine
                       return (x:xs)

-- type World = ...
-- type IO a = World -> (a, World)

-- IO에 의해서 World가 변경됨
