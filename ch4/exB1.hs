-- file: ch4/exB1.hs
-- A function that converts a `String` to an `Int`
-- Daniel Brice

import Data.Char (digitToInt)

safeDigitToInt :: Char -> Int
safeDigitToInt '.' = error "safeDigitToInt: '.' is not an allowed input."
safeDigitToInt dig = digitToInt dig

safeAsInt :: String -> Int
safeAsInt ('-' : xs) = (- 1) * (safeAsInt xs)
safeAsInt ""         = error "safeAsInt: \"\" is not an allowed input."
safeAsInt xs         = foldl step 0 xs
  where
    step acc dig = acc * 10 + (safeDigitToInt dig)
