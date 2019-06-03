module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
    | n < 1     = Nothing
    | otherwise = Just (nth' [] n 2)

nth' :: [Integer] -> Int -> Integer -> Integer
nth' primes it nextCandidate
    | it == 0 = head primes
    | otherwise = if (isPrime nextCandidate)
        then nth' (nextCandidate:primes) (it-1) (nextCandidate+1)
        else nth' primes it (nextCandidate+1)
    where
        isPrime c = (length [x | x<-primes, x `mod` c == 0] == 0)