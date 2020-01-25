module Primes where

primes :: [Int]
primes = sieve [2 .. 10000]

sieve :: [Int] -> [Int]
sieve []                 = []
sieve (nextPrime : rest) = nextPrime : sieve noFactors
  where noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

data PrimeError = TooLarge | InvalidValue deriving Eq
instance Show PrimeError where
  show TooLarge     = "Number is too large"
  show InvalidValue = "Wrong number"

displayResult :: Either PrimeError Bool -> String
displayResult (Right True      ) = "It's prime number"
displayResult (Right False     ) = "It's not prime number"
displayResult (Left  primeError) = show primeError

isPrime :: Int -> Either PrimeError Bool
isPrime n | n < 2              = Left InvalidValue
          | n >= length primes = Left TooLarge
          | otherwise          = Right (n `elem` primes)

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 []              = []
unsafePrimeFactors n []              = []
unsafePrimeFactors n (next : primes) = if n `mod` next == 0
  then next : unsafePrimeFactors (n `div` next) (next : primes)
  else unsafePrimeFactors n primes

primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2 = Nothing
               | n >= length primes = Nothing
               | otherwise = Just (unsafePrimeFactors n primesLessThanN)
  where primesLessThanN = filter (<= n) primes
