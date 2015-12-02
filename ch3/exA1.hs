-- File: ch3/exA1.hs
-- A user-implemented List struct, and a pair of conversion functions.

-- | A user-implemented replacement for `[a]`.
data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList :: [a] -> List a
-- ^ Converts a standard `[a]` into a `List a`.
fromList (x : xs) = Cons x $ fromList xs
fromList []       = Nil

toList :: List a -> [a]
-- ^ Converts a custom `List a` into a `[a]`.
toList (Cons x xs) = x : toList xs
toList Nil   	   = []
