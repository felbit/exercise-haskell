module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | not equalLength = Nothing
  | otherwise = Just $ sum $ map areEqualLetters $ zip xs ys
  where
    equalLength = length xs == length ys
    areEqualLetters (x, y) = if x == y then 0 else 1
