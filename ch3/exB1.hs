-- File: ch3/exB1.hs
-- A function that computes the number of elements in a list.

length' :: [a] -> Int
length' (x:xs) = 1 + (length' xs)
length' []     = 0
