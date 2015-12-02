-- File: ch2/ex2.hs
-- A function that returns the second-to-last element of a list.

lastButOne :: [a] -> a
lastButOne = last . init
