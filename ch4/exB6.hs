-- file: ch4/exB6.hs
-- A reimplementation of common prelude functions using folds.
-- Daniel Brice

any' :: (a -> Bool) -> [a] -> Bool
any' test xs = foldr (\x acc -> test x || acc) False xs

cycle' :: [a] -> [a]
cycle' = undefined

words' :: String -> [String]
words' = undefined

unlines' :: [String] -> String
unlines' ls = foldr (\l1 l2 -> l1 ++ "\n" ++ l2) [] ls
