module AllYourBase (Error(..), rebase) where

import Data.List (genericTake, find, foldl)
import Data.Maybe (fromJust)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits =
  case (isValidBase inputBase, isValidBase outputBase, invalidDigit inputBase inputDigits) of
    (False, _, _) -> Left InvalidInputBase
    (_, False, _) -> Left InvalidOutputBase
    (_, _, Just x) -> Left $ InvalidDigit x
    _ -> Right $ digits outputBase $ unDigits inputBase inputDigits
  where
    isValidBase n = n > 1
    invalidDigit base = find (\x -> x `mod` base /= x)

unDigits :: Integral n => n -> [n] -> n
unDigits base = foldl (\ a b -> a * base + b) 0

digits :: Integral n => n -> n -> [n]
digits base = reverse . fromJust . mDigitsRev base

mDigitsRev :: Integral n => n -> n -> Maybe [n]
mDigitsRev base i =
  if base < 1
  then Nothing
  else Just $ dr base i
    where
      dr _ 0 = []
      dr b x = case base of
                1 -> genericTake x $ repeat 1
                _ -> let (rest, lastDigit) = quotRem x b
                     in lastDigit : dr b rest
