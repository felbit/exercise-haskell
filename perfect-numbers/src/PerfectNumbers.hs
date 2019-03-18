module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient
                    | Perfect
                    | Abundant
                    deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n 
  | n <  1    = Nothing
  | m >  n    = Just Abundant
  | m <  n    = Just Deficient
  | otherwise = Just Perfect
  where f = filter (\x -> n `rem` x == 0) [1..(n `div` 2)]
        m = sum f
