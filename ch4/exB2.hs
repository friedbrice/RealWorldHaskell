-- file: ch4/exB2.hs
-- A safe function that converts a `String` to an `Int`
import Data.Char (digitToInt)

type ErrorMessage = String

safeAsInt :: String -> Either ErrorMessage Int
safeAsInt "" = Left "Empty input."
safeAsInt digs | nonDigits digs = Left "Non-digits."
               | otherwise      = Right $ safeAsInt' digs
  where
    nonDigits digs = not . all (`elem` "-0123456789") $ digs
    safeAsInt' ('-' : xs) = (- 1) * (safeAsInt' xs)
    safeAsInt' xs         = foldl step 0 xs
    step acc dig = acc * 10 + (digitToInt dig)
