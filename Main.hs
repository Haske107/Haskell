module Main where


hello :: IO ()
hello = putStrLn "Hi. What's your name?"

comeOn :: String -> IO ()
comeOn name = putStrLn $ "Hey " ++ name ++ ", come here often?"

main :: IO ()
main = do
  hello
  name <- getLine
  comeOn name
