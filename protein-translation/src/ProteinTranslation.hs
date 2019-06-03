module ProteinTranslation(proteins) where

proteins :: String -> Maybe [String]
proteins xs
  | length xs < 3 = Nothing
  | otherwise     = Just $ proteins' xs

proteins' :: String -> [String]
proteins' [] = []
proteins' ('A':'U':'G':xs) = "Methionine" : proteins' xs
proteins' ('U':'U':'U':xs) = "Phenylalanine" : proteins' xs
proteins' ('U':'U':'C':xs) = "Phenylalanine" : proteins' xs
proteins' ('U':'U':'A':xs) = "Leucine" : proteins' xs
proteins' ('U':'U':'G':xs) = "Leucine" : proteins' xs
proteins' ('U':'C':'U':xs) = "Serine" : proteins' xs
proteins' ('U':'C':'C':xs) = "Serine" : proteins' xs
proteins' ('U':'C':'A':xs) = "Serine" : proteins' xs
proteins' ('U':'C':'G':xs) = "Serine" : proteins' xs
proteins' ('U':'A':'U':xs) = "Tyrosine" : proteins' xs
proteins' ('U':'A':'C':xs) = "Tyrosine" : proteins' xs
proteins' ('U':'G':'U':xs) = "Cysteine" : proteins' xs
proteins' ('U':'G':'C':xs) = "Cysteine" : proteins' xs
proteins' ('U':'G':'G':xs) = "Tryptophan" : proteins' xs
proteins' ('U':'A':'A':_) = []
proteins' ('U':'A':'G':_) = []
proteins' ('U':'G':'A':_) = []
proteins' _ = []
