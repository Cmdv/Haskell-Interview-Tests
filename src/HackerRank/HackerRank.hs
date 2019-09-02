module HackerRank.HackerRank where

import           Control.Monad
import qualified Data.Matrix   as MX
import qualified Data.Text     as T
import qualified Data.Vector   as V

-- | COMPARE THE TRIPLETS
compareTriplets :: [Int] -> [Int] -> [Int]
compareTriplets a b = do
  let result =
        foldl whoGetsPoints (0,0) $ zip a b
  pairToList result

whoGetsPoints :: (Int,Int) -> (Int, Int) -> (Int, Int)
whoGetsPoints (a1,b1) (x,y) | x > y = (a1 + 1, b1)
                            | x < y = (a1, b1 + 1)
                            | otherwise = (a1, b1)

pairToList :: (a, a) -> [a]
pairToList (x,y) = [x,y]


-- DIAGONAL DIFFERENCE

diagonalDifference :: [Int] -> Int
diagonalDifference (x : xs) = do
  let nMatrix = MX.fromList x x xs
      diagLR = MX.trace nMatrix
      diagRL = traceOp nMatrix
  diff diagLR diagRL
  where
    diff a b | a > b = a - b
             | otherwise = b - a

-- | this can only work on a square matrix
-- | It is the oposite diagonal that doesn't exist in Matrix library
getOpDiag :: MX.Matrix a -> V.Vector a
getOpDiag m = do
  let nRows = MX.nrows m
      nCols = MX.ncols m
      k     = min nRows nCols
  V.generate k $ \i -> m MX.! (nCols - i ,i+1)

traceOp :: Num a => MX.Matrix a -> a
traceOp = V.sum . getOpDiag

-- | PLUSMINUS

plusMinus :: [Int] -> [Float]
plusMinus arr = do
  let arrLength = length arr
      p = getPer (> 0) arr arrLength
      n = getPer (< 0) arr arrLength
      o = getPer (== 0) arr arrLength
  [p,n,o]
  where
    getPer p ar arrL = fromIntegral ((length . filter p) ar) / fromIntegral arrL
