
module Main where
import Data.Char
main = do
  line <- getContents
  putStrLn $ map toUpper line
