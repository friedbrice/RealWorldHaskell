-- file: ch4/exA4.hs
-- A program that transposes a file.
-- Daniel Brice

import Data.List (transpose)
import System.Environment (getArgs)

transposeLines :: String -> String
transposeLines = unlines . transpose . lines

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith f inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (f input)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input,output] -> interactWith transposeLines input output
    _              -> putStrLn "error: exactly two arguments needed"
