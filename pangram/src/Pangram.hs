module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)

qsort :: [Char] -> [Char]
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y < x] ++ (x : qsort [y | y <- xs, y > x])

isPangram :: String -> Bool
isPangram text = alphabet == qsort lcAlpha
  where
    lcAlpha  = filter isAlpha $ map toLower text
    alphabet = ['a'..'z']
