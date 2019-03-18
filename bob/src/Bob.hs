{-# LANGUAGE OverloadedStrings #-}
module Bob (responseFor) where

import qualified Data.Char as C
import qualified Data.Text as T
import           Data.Text (Text)

responseFor :: Text -> Text
responseFor xs
  | isSilence          = "Fine. Be that way!"
  | isForcefulQuestion = "Calm down, I know what I'm doing!"
  | isShouting         = "Whoa, chill out!"
  | isQuestion         = "Sure."
  | otherwise          = "Whatever."
  where
    hasAlpha           = T.any $ C.isAlpha
    isShouting         = hasAlpha xs && T.toUpper xs == xs
    isQuestion         = T.last (T.stripEnd xs) == '?'
    isForcefulQuestion = isShouting && isQuestion
    isSilence          = T.null $ T.strip xs
