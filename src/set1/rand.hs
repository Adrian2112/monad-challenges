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

randLetter :: Seed -> (Char, Seed)
randLetter seed = (toLetter random, nextSeed)
  where
    (random, nextSeed) = rand seed

nRandsWithFn :: Integer -> (Seed -> (a, Seed)) -> Seed -> [(a, Seed)]
nRandsWithFn 0 _ _ = []
nRandsWithFn n fn seed = newRand : nRandsWithFn (n-1) fn nextSeed
  where
    newRand@(_, nextSeed) = fn seed

fiveRands' :: [Integer]
fiveRands' = map fst $ nRandsWithFn 5 rand initialSeed

randString3 :: String
randString3 = map fst $ nRandsWithFn 3 randLetter initialSeed

randString3' :: String
randString3' = map (toLetter . fst) $ nRands 3 initialSeed

