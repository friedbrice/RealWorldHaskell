-- file: ch4/exA1.hs
-- Safe versions of some basic list manipulation functions.

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead _       = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (_ : xs) = Just xs
safeTail _        = Nothing

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just . head . reverse $ xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just . reverse . tail . reverse $ xs
