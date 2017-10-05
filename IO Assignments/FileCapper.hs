module Main where
import Data.Char
main = do
  s <- readFile "Sample.txt"
  putStrLn $ map toUpper s
