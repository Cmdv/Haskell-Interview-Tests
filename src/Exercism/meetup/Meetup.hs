module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday deriving (Eq, Enum, Show)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth deriving (Enum, Show)

dayOfWeek :: Day -> Weekday
dayOfWeek d = toEnum (d' - 1)
  where (_, _, d') = toWeekDate d

weekdaysInMonth :: Weekday -> Integer -> Int -> [Day]
weekdaysInMonth w y m = filter (\d -> dayOfWeek d == w)
  (take (gregorianMonthLength y m) [fromGregorian y m 1..])

dayOfMonth :: Day -> Int
dayOfMonth d = d'
  where (_, _, d') = toGregorian d

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = case schedule of
  Last   -> last wdays
  Teenth -> last $ filter (\d -> (dayOfMonth d) `elem` [10..19]) wdays
  _      -> wdays !! fromEnum schedule
  where wdays = weekdaysInMonth weekday year month
