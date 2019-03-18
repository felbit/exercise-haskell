module LeapYear (isLeapYear) where

dividableBy :: Integer -> Integer -> Bool
dividableBy year n = year `rem` n == 0

isLeapYear :: Integer -> Bool
isLeapYear year
  | year `dividableBy` 400 = leap
  | year `dividableBy` 100 = notLeap
  | year `dividableBy`   4 = leap
  | otherwise              = notLeap
  where leap    = True
        notLeap = False

