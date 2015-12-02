-- File: ch3/exB3.hs
-- A function that computes the mean of a list.

mean :: Floating a => [a] -> a
-- ^ Walks the whole list twice, and then divides.
mean xs = (sum xs) / (fromIntegral $ length xs)

mean' :: Floating a => [a] -> a
-- ^ Walks the list once.
-- Despite that `mean'` walks the list only once whereas `mean` walks
-- the list twice, `mean` still appears to be faster. This is probably
-- because the `sum` and `length` functions from the Prelude are well
-- optimized.
mean' = meaner 0 0
  where meaner n s (x : xs) = meaner (n + 1) (s + x) xs
        meaner n s []       = s / n

--mean'' :: Floating a => [a] -> a
---- ^ Strict version of `mean'`
---- And now we won't compile :-(
--mean'' = meaner 0 0
--  where meaner n s []       = s / n
--        meaner n s (x : xs) = let n' = n + 1
--                                  s' = s + x
--                              in  seq n' $ seq s' $ meaner n' s' xs
