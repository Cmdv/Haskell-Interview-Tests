module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where


import Data.Vector (Vector)
import qualified Data.Vector as V

data Matrix a = Matrix (Vector (Vector a)) deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix m)
  | V.null m = 0
  | otherwise = V.length $ V.head m

column :: Int -> Matrix a -> Vector a
column x (Matrix m) = flip (V.!) (x -1) <$> m

flatten :: Matrix a -> Vector a
flatten (Matrix m) = V.foldr1 (V.++) m

fromList :: [[a]] -> Matrix a
fromList xss = Matrix $ V.fromList $ V.fromList <$> xss

fromString :: Read a => String -> Matrix a
fromString = fromList . map (map read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) m =
  Matrix $ V.map (\x -> V.slice x c flatm) (V.fromList [0, c..area])
  where
    flatm = flatten m
    area = r * (c - 1)

row :: Int -> Matrix a -> Vector a
row x (Matrix m) = m V.! (x - 1)

rows :: Matrix a -> Int
rows (Matrix m) = V.length m

shape :: Matrix a -> (Int, Int)
shape m = (rows m,  cols m)

transpose :: Matrix a -> Matrix a
transpose m = Matrix $ V.generate (cols m) (\i -> column (i + 1) m)
