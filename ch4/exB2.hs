-- file: ch4/exB2.hs
-- A safe function that converts a `String` to an `Int`
-- Daniel Brice

import Data.Char (digitToInt)

type ErrorMessage = String

safeAsInt :: String -> Either ErrorMessage Int
safeAsInt "" = Left "Empty input."
safeAsInt digs | nonDigits digs = Left "Non-digits."
               | infixSubt digs = Left "Infixed negative sign"
               | otherwise      = Right $ safeAsInt' digs
  where
    nonDigits             = not . all (`elem` "-0123456789")
    infixSubt             = ('-' `elem`) . tail
    safeAsInt' ('-' : xs) = (- 1) * (safeAsInt' xs)
    safeAsInt' xs         = foldl step 0 xs
    step acc dig          = acc * 10 + (digitToInt dig)
