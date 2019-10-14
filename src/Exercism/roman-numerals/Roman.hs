module Roman (numerals) where

import qualified Data.Map as Map

numerals :: Integer -> Maybe String
numerals n
  | n == 0 = Just ""
  | n > 3000 || n < 0 = Nothing
  | otherwise =  do
      let val = maximum $ filter (<= n) (Map.keys romanNumbers)
      (++) <$> Map.lookup val romanNumbers <*> numerals (n - val)

romanNumbers :: Map.Map Integer String
romanNumbers = Map.fromList
  [ (1, "I")
  , (4, "IV")
  , (5, "V")
  , (9, "IX")
  , (10, "X")
  , (40, "XL")
  , (50, "L")
  , (90, "XC")
  , (100, "C")
  , (400, "CD")
  , (500, "D")
  , (900, "CM")
  , (1000, "M")
  ]
