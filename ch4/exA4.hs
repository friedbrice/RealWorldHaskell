-- file: ch4/exA4.hs
-- A program that transposes the text of a file.
import Data.List (transpose)
import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = myFunction'

myFunction' :: String -> String
myFunction' = unlines . transpose . lines
