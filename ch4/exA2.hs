-- file: ch4/exA2.hs
-- A function that acts similarly to `words`, but takes a predicate and a
-- list of any type, and splits its input list on every element for which
-- the predicate returns `False`.

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs =
  let (pre, suf) = break (not . p) xs
  in pre : (splitWith p $ tail suf)
