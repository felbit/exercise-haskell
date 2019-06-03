module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse fromDNA
  where
    fromDNA :: Char -> Either Char Char
    fromDNA 'C' = Right 'G'
    fromDNA 'G' = Right 'C'
    fromDNA 'T' = Right 'A'
    fromDNA 'A' = Right 'U'
    fromDNA  x  = Left x
