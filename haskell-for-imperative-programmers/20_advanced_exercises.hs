data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

cut :: Integer -> Tree a -> Tree a
cut 0 _ = Leaf
cut _ Leaf = Leaf
cut n (Node l x r) = Node (cut (n-1) l) x (cut (n-1) r)

inv_tup_tree :: Tree (Integer, Integer)
inv_tup_tree = gen (0,0)
  where gen (x,y) = Node (gen (x+1,y)) (x,y) (gen (x,y+1))

mockTree :: Tree (Integer, Integer)
mockTree = Node
  (Node
    (Node Leaf (2,0) Leaf)
    (1,0)
    (Node Leaf (1,1) Leaf))
  (0,0)
  (Node
    (Node Leaf (1,1) Leaf)
    (0,1)
    (Node Leaf (0,2) Leaf))
