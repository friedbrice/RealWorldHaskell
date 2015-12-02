-- File: ch3/exB4.hs
-- A function that palindromizes a list

palindromize :: [a] -> [a]
-- ^ I feel like this is cheating, because Ch 3 hasn't introduced the
--   standard list manipulation functions yet.
palindromize xs = xs ++ (reverse xs)

palindromize' :: [a] -> [a]
-- ^ I have no idea how to do this without just implementing my own
--   `reverse` function.
palindromize' [] = []
palindromize' xs = xs ++ (rev . tail $ xs) ++ (head xs : [])
  where rev (x : xs) = (rev xs) ++ (x : [])
        rev []       = []
