module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size > l || size < 0 = Left InvalidSpan
  | otherwise = do
      let dInts = (\x -> if isDigit x then Right (digitToInt x) else Left x) <$> digits
      productOfdsg l size <$> isDigitError dInts
  where l = length digits

productOfdsg :: (Integral a, Num c) => Int -> Int -> [a] -> c
productOfdsg l size dgs =
  fromIntegral . maximum
   $ fmap (\i -> (product . take size . drop i) dgs) [0..l - size]

isDigitError :: [Either Char Int] -> Either Error [Int]
isDigitError = traverse (either (Left . InvalidDigit) Right)
