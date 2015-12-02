-- file: ch4/exB4.hs
-- A pair of function that take from a list while a predicate is true.

takeWhileRecursive :: (a -> Bool) -> [a] -> [a]
takeWhileRecursive = loop []
  where
    loop acc p (x : xs) = if p x
                          then loop (acc ++ [x]) p xs
                          else acc
    loop acc _ _        = acc

takeWhileFold :: (a -> Bool) -> [a] -> [a]
takeWhileFold p = reverse $ foldr step []
  where
    step x acc = if p x
                 then acc ++ [x]
                 else []
