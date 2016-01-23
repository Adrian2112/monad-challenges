{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = (Seed -> (a, Seed))

initialSeed :: Seed
initialSeed = mkSeed 1

fiveRands :: [Integer]
fiveRands = map fst $ nRands 5 initialSeed

nRands :: Integer -> Seed -> [(Integer, Seed)]
nRands 0 _ = []
nRands n seed = newRand : nRands (n-1) nextSeed
  where
    newRand@(_, nextSeed) = rand seed

randLetter :: Gen Char
randLetter seed = (toLetter random, nextSeed)
  where
    (random, nextSeed) = rand seed

nRandsWithFn :: Integer -> Gen a -> Seed -> [(a, Seed)]
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

-- rand :: Gen Integer

randEven :: Gen Integer
--randEven seed = (even, nextSeed)
--  where
--    (random, nextSeed) = rand seed
--    even = random * 2
randEven = generalA (* 2) rand

randOdd :: Gen Integer
--randOdd seed = (odd, nextSeed)
--  where
--    (random, nextSeed) = randEven seed
--    odd = random + 1
randOdd = generalA (+ 1) randEven

randTen :: Gen Integer
--randTen seed = (ten, nextSeed)
--  where
--    (random, nextSeed) = rand seed
--    ten = random * 10
randTen = generalA (* 10) rand

generalA :: (a -> a) -> Gen a -> Seed -> (a, Seed)
generalA transformer generator seed = (transformer random, nextSeed)
  where
    (random, nextSeed) = generator seed

randPair :: Gen (Char, Integer)
randPair seed = ((char, number), lastSeed)
  where
    (char, nextSeed) = randLetter seed
    (number, lastSeed) = rand nextSeed

generalPair :: Gen a -> Gen b -> Gen (a,b)
--generalPair genA genB seed = ((randA, randB), lastSeed)
--  where
--    (randA, nextSeed) = genA seed
--    (randB, lastSeed) = genB nextSeed
generalPair genA genB seed = generalB genA genB (,) seed

generalB :: Gen a -> Gen b -> (a -> b -> c) -> Gen c
generalB genA genB constructor seed = ((constructor randA randB), lastSeed)
  where
    (randA, nextSeed) = genA seed
    (randB, lastSeed) = genB nextSeed
