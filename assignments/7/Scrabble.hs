{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int deriving (Eq, Show, Num, Ord)

getScore :: Score -> Int
getScore (Score n) = n

instance Monoid Score where
    mempty = Score 0
    mappend = (+)

score :: Char -> Score
score = Score . f . toUpper
    where f c | c `elem` "AEILNORSTU" = 1
              | c `elem` "DG" = 2
              | c `elem` "BCMP" = 3
              | c `elem` "FHVWY" = 4
              | c `elem` "K" = 5
              | c `elem` "JX" = 8
              | c `elem` "QZ" = 10
              | otherwise = 0

scoreString :: String -> Score
scoreString = mconcat . map score

