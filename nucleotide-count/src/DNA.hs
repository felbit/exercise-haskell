module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = Right (fromList [ (A, 0)
                                      , (C, 0)
                                      , (G, [x | x <- xs,  ])
                                      , (T, 0) ])
