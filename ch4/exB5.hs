-- file: ch4/exB5.hs
-- A user implementation of `groupBy`

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _    []       = []
groupBy test list@(x : xs) = ys : (groupBy test zs)
  where
    ys = takeWhile (test x) list
    zs = dropWhile (test x) list

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' test = foldl step []
  where
    step acc x = 
