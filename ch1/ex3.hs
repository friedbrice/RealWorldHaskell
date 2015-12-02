-- File: ch1/ex3.hs
-- A program that counts the number of words (space-seperated strings).
-- Reads from stdin.
-- Write to stdout.

wordCount :: String -> String
wordCount x = (show . length . words $ x) ++ "\n"

main = interact wordCount
