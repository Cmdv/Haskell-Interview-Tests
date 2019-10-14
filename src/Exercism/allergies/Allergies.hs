module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits (testBit)

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum)

allergies :: Int -> [Allergen]
allergies s = filter (flip isAllergicTo s) [Eggs .. Cats]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo a s = testBit s $ fromEnum a
