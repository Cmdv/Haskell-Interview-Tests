module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors x = prime x 2
  where
    prime :: Integer -> Integer -> [Integer]
    prime n curr
      | modN == 0  = curr : primeFactors divN
      | otherwise = prime n (curr + 1)
      where (divN, modN) = divMod n curr
