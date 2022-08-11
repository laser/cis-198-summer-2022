{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Homework.Week11.Assignment (
  Battlefield(..),
  battle,
  invade,
  successProb
) where

import Control.Monad
import Control.Monad.Random

import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1, 6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

-- #2 
-- Simulate a single battle between two opposing armies. 
-- Roll the dice, interpret the results, and update the
-- two armies to reflect any casualties.
dice :: Int -> Rand StdGen [DieValue]
dice = flip replicateM die

applyRolls :: Ord a => Battlefield -> (a, a) -> Battlefield
applyRolls field (attack, defense)
  | attack > defense = field { defenders = defenders field - 1 } -- attacker wins
  | otherwise        = field { attackers = attackers field - 1 } -- defender wins

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield attack defense) = do
  let roll = \n -> flip replicateM die n

  let effectiveAttackers = min 3 (attack - 1)
  let effectiveDefenders = min 2 defense

  attackRolls  <- sort <$> roll effectiveAttackers
  defenseRolls <- sort <$> roll effectiveDefenders

  let rolls = zip attackRolls defenseRolls

  return $ foldl' applyRolls bf rolls

-- #3
-- Simulate repeated battles, ending only when one
-- side runs out of combatants.
invade :: Battlefield -> Rand StdGen Battlefield
invade field@(Battlefield attack defense)
  | defense == 0 = return field
  | attack  <= 1 = return field
  | otherwise    = battle field >>= \b -> invade b

-- #4
isSuccess :: Battlefield -> Bool
isSuccess = (== 0) . defenders

successProb :: Battlefield -> Rand StdGen Double
successProb = fmap ((/ 1000) . genericLength . filter isSuccess) . replicateM 1000 . invade