module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen)

data Character = Character
  { name         :: String
  , strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier c = (if (c - 10) `mod` 2 == 0 then (c - 10) else (c - 11)) `div` 2

ability :: Gen Int
ability =
  error "You need to implement this generator."

character :: Gen Character
character =
  error "You need to implement this generator."
