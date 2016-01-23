{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

initialSeed :: Seed
initialSeed = mkSeed 1

fiveRands :: [Integer]
fiveRands = map fst $ nRands 5 initialSeed

nRands :: Integer -> Seed -> [(Integer, Seed)]
nRands 0 _ = []
nRands n seed = newRand : nRands (n-1) nextSeed
  where
    newRand@(_, nextSeed) = rand seed
