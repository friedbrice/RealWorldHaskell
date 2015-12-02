-- file: ch4/exB3.hs
-- A function that concats a list of lists into a single list.

listJoin :: [[a]] -> [a]
listJoin = foldl (++) []
