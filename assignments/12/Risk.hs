{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }


-- Exercise 2

rollTimes :: Army -> Rand StdGen [DieValue]
rollTimes 0 = return []
rollTimes n = do
  d <- die
  rest <- rollTimes (n-1)
  return (d : rest)

sortRev :: Rand StdGen [DieValue] -> Rand StdGen [DieValue]
sortRev = liftM $ sortBy (flip compare)


computeCasulty :: [(DieValue,DieValue)] -> (Int,Int)
computeCasulty [] = (0,0)
computeCasulty ((x1,x2):xs) =
    let (xs1,xs2) = computeCasulty xs in
    if x1 <= x2 then (1+xs1,xs2) else (xs1,1+xs2)

battle :: Battlefield -> Rand StdGen Battlefield
battle Battlefield { attackers = att, defenders = def } = do
  attRolls <- sortRev . rollTimes $ min att 3
  defRolls <- sortRev . rollTimes $ min def 2
  let (x,y) = computeCasulty $ zip attRolls defRolls
  return Battlefield { attackers = att-x, defenders = def-y }


-- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade b | attackers b < 2 || defenders b == 0 = return b
         | otherwise = (battle b) >>= invade


-- Exercise 4

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  results <- replicateM 1000 (invade b)
  let n = length . filter (\x -> (defenders x) == 0) $ results
  return (n / 1000)

