-- file: ch1/ex4.hs
-- A program that counts the number of characters.
-- Reads from stdin.
-- Write to stdout.

charCount :: String -> String
charCount x = (show . length $ x) ++ "\n"

main = interact charCount
