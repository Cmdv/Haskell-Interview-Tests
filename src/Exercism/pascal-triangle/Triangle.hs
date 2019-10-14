module Triangle (rows) where

pascal :: Int -> [[Integer]]
pascal 0 = []
pascal x = take x $ iterate next [1]

next :: Num a => [a] -> [a]
next xs = zipWith (+) ([0] : xs) (xs : [0])
