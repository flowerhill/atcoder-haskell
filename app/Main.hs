module Main where

main :: IO ()
main = do
  s <- getLine

  let s1 = dropWhile (== 'C') $ dropWhile (== 'B') $ dropWhile (== 'A') s

  putStrLn $ if s1 == "" then "Yes" else "No"