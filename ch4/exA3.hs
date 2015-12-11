-- file: ch4/exA3.hs
-- A program that prints the first word of each line of its input.
-- Daniel Brice

import Data.Maybe (catMaybes)
import System.Environment (getArgs)

firstWord :: String -> String
firstWord = unlines . catMaybes . map (safeHead . words) . lines
  where
    safeHead (x : _) = Just x
    safeHead _       = Nothing

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith f inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (f input)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input,output] -> interactWith firstWord input output
    _              -> putStrLn "error: exactly two arguments needed"
