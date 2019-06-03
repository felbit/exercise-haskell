module IsbnVerifier (isbn) where

import Data.Char (isAlphaNum, isDigit, digitToInt)

isbn :: String -> Bool
isbn xs = lengthCheck && (sumCheck $ sum $ lastDigit : isbn' digits 10)
  where
    lengthCheck = 10 == (length $ filter isAlphaNum xs)
    digits      = map digitToInt $ filter isDigit xs
    lastDigit   = if last xs == 'X' then 10 else 0
    sumCheck s  = s `rem` 11 == 0

isbn' :: [Int] -> Int -> [Int]
isbn' [] _ = []
isbn' xs p = (head xs) * p : isbn' (tail xs) (p - 1)
