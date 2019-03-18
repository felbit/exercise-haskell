module Acronym (abbreviate) where

import Data.Char (toUpper)

removePunx :: String -> String
removePunx xs = map (\c -> if c `elem` ".,:;-_!?" then ' ' else c) xs
  
initialsAndUpper :: [String] -> String
initialsAndUpper []     = []
initialsAndUpper (x:xs) = (head x : findUpper (tail x)) ++ initialsAndUpper xs
  where
    isAllUpper w = (map toUpper w) == w
    findUpper  t = if isAllUpper t then [] else [c | c <- t, c `elem` ['A'..'Z']]

allToUpper :: String -> String
allToUpper xs = map toUpper xs

abbreviate :: String -> String
abbreviate xs = allToUpper (initialsAndUpper (words (removePunx xs)))
