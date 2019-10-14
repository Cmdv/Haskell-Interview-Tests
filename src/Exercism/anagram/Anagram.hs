module Anagram (anagramsFor) where

import Control.Monad
import Data.Char (toLower)
import Data.Function
import Data.List (sort, isSuffixOf, sortOn)
import Data.Maybe

anagramsFor :: String -> [String] -> [String]
anagramsFor x = filter (\x' -> x /= x' && ((==) `on` sortOn toLower) x x')

-- anagramsFor :: String -> [String] -> [String]
-- anagramsFor xs xss =
--   catMaybes $ checkAnagram xs <$> xss


checkAnagram :: String -> String -> Maybe String
checkAnagram sub cand = do
  let subLower  = toLower <$> sub
      candLower = toLower <$> cand
  guard (subLower /= candLower)
  guard (isSuffixOf (sort subLower) (sort candLower))
  pure cand
