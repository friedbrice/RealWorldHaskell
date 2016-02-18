-- file: ch4/exB5.hs
-- A user implementation of `groupBy`
-- Daniel Brice

groupByRecursive :: (a -> a -> Bool) -> [a] -> [[a]]
groupByRecursive test list = loop [] test list
  where
    loop acc _ []         = acc
    loop acc t l@(x : xs) = let (ys, zs) = break (not . t x) xs
                            in loop (acc ++ [x : ys]) t zs
