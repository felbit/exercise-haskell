module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter isAnagram xss
  where
    sortCI l = sort (map toLower l)
    notEqualCI w1 w2 = map toLower w1 /= map toLower w2
    isAnagram w = sortCI w == sortCI xs && notEqualCI w xs
