module Phone (number) where

import Data.List (filter)
import Data.Char (isNumber)

number :: String -> Maybe String
number xs
  | length digits == 10 && is2to9 (head digits) && is2to9 (digits !! 3) = Just digits
  | length digits == 11 && head digits == '1'                           = number (tail digits)
  | otherwise                                                           = Nothing
  where
    digits = filter isNumber xs
    is2to9 = \x -> elem x ['2'..'9']
