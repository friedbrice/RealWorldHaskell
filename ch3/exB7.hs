-- File: ch3/exB7.hs
-- A function that joins a list of lists together using a given separator value.

intersperse :: a -> [[a]] -> [a]
intersperse _ []         = []
intersperse _ [xs]       = xs
intersperse c (xs : xss) = xs ++ [c] ++ (intersperse c xss)
