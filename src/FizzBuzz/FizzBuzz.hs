module FizzBuzz.FizzBuzz where

import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.DList                as DL

fizzBuzz :: Int -> String
fizzBuzz n | mod n 15 == 0 = "FizzBuzz"
           | mod n 5 == 0 = "Buzz"
           | mod n 3 == 0 = "Fizz"
           | otherwise    = show n

runFizzBuzz :: IO ()
runFizzBuzz = mapM_ (putStrLn . fizzBuzz) [1..100]


-- With State
fizzBuzzList :: [Int] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []


addResult :: Int -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

runStateFizzBuzz :: IO ()
runStateFizzBuzz =
  mapM_ putStrLn $
    reverse $ fizzBuzzList [1..100]


-- Using Dlist
fizzBuzzList' :: [Int] -> DL.DList String
fizzBuzzList' list = execState (mapM_ addResult' list) DL.empty

addResult' :: Int -> State (DL.DList String) ()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)
