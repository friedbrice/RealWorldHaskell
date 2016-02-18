-- file: ch4/exB6.hs
-- A reimplementation of common prelude functions using folds.
-- Daniel Brice

any' :: (a -> Bool) -> [a] -> Bool
any' test xs = foldr (\x acc -> test x || acc) False xs

cycle' :: [a] -> [a]
cycle' [] = error "empty list"
cycle' xs = foldr (:) (cycle' xs) xs

myWords :: String -> [String]
myWords str = helper [] str
  where
    helper acc (char : chars) = case char of ' ' -> acc ++ helper [] chars
                                             _   -> helper (char : acc) chars
    -- helper :: [Char] -> [Char] -> [[Char]]

words' :: String -> [String]
words' xs = foldr step [] xs
  where
    step x acc = undefined

unlines' :: [String] -> String
unlines' ls = foldr (\l1 l2 -> l1 ++ "\n" ++ l2) [] ls
