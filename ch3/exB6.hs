-- File: ch3/exB6.hs
-- A function that sorts a list of lists by length
import Data.List (sortBy)

sortByLength :: [[a]] -> [[a]]
-- I feel like using `sortBy` is cheating, but the authors suggest it.
sortByLength = sortBy compareLength
  where compareLength xs ys = compare (length xs) (length ys)
