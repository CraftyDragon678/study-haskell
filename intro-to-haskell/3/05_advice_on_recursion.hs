-- define the type
init :: [a] -> [a]

-- enumerate the cases
init (x: xs)

-- define the simple case
init (x: xs) | null xs = []
             | otherwise = 

-- define the other cases
init (x: xs) | null xs = []
             | otherwise = x : init xs

-- same as
init :: [a] -> [a]
init [] = []
init (x:xs) = x : init xs

