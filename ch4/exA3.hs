-- file: ch4/exA3.hs
-- A program that prints the first word of each line of its input.
import Data.Maybe (catMaybes)
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
myFunction' = unlines . catMaybes . map (safeHead . words) . lines

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead _       = Nothing
