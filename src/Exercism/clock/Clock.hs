module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int deriving (Eq)

instance Show Clock where
  show (Clock c) = s h <> ":" <> s m
    where
      (h, m) = divMod c 60
      s x    = do
        let (t, o) = divMod x 10
        show t <> show o

fromHourMin :: Int -> Int -> Clock
fromHourMin hour mins = Clock $ flip mod 1440 $ hour * 60 + mins

toString :: Clock -> String
toString = show

addDelta :: Int -> Int -> Clock -> Clock
addDelta hours mins (Clock c) = fromHourMin hours $ mins + c
