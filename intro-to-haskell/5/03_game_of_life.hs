import Control.Concurrent

seqn :: [IO ()] -> IO ()
seqn [] = return ()
seqn (x:xs) = do x
                 seqn xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt :: Pos -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)
type Board = [Pos]

showCells :: Board -> IO ()
showCells b = seqn [writeAt p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not $ isAlive b p

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x+i,y+j) | i <- [-1..1], j <- [-1..1],
                                          i /= 0 || j /= 0]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height + 1))
-- 순수 함수가 아님? -> width랑 height도 함수다!!!!!!

liveNeighbs :: Board -> Pos -> Int
liveNeighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveNeighbs b p) [2,3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups $ concat $ map neighbs b,
                    isEmpty b p,
                    liveNeighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

nextGen :: Board -> Board
nextGen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showCells b
            threadDelay 50000
            life $ nextGen b

-- wait :: Int -> IO ()
-- wait n = seqn [return () | _ <- [1..n]]

width :: Int
width = 20

height :: Int
height = 20

run :: IO ()
run = life glider

glider :: Board
-- glider = [(4,2),(5,3),(5,4),(4,4),(3,4)]
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]