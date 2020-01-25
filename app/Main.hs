module Main where

import           Primes

main :: IO ()
main = do
  putStrLn "Enter a number for prime check"
  n <- read <$> getLine
  let result = isPrime n
  putStrLn (displayResult result)
