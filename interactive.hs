-- Some templates for writing interactive programs

myAwesomeFunction :: String -> String
myAwesomeFunction = undefined

main1 :: IO ()
main1 = interact myAwesomeFunction
-- This version will create a pipe-able command-line utility.

main2 :: IO ()
main2 = interact $ unlines . map myAwesomeFunction . lines
-- This version will create an interactive command interpreter.
