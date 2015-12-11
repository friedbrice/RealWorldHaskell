-- file: ch4/exB5.hs
-- A user implementation of `groupBy`
-- Daniel Brice

groupByRecursive :: (a -> a -> Bool) -> [a] -> [[a]]
groupByRecursive _    []         = []
groupByRecursive test xs@(x : _) = ys : (groupByRecursive test zs)
  where
    ys = takeWhile (test x) xs
    zs = dropWhile (test x) xs

groupByFold :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFold test xs = foldl step [] xs
  where
    step (ys : _) x = case ys of []      -> 
                                 (y : _) -> 
    step []       x = [[x]]