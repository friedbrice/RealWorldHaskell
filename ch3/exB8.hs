-- File: ch3/exB8.hs
-- A function that determined the height of a tree

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height :: Tree a -> Int
height Empty        = 0
height (Node x l r) = 1 + (max (height l) (height r))
