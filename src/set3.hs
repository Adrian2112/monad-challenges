{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs xs ys = concat $ map (\x -> map ((,) x) ys) xs

data Card = Card Int String

instance Show Card where
  show (Card r s) = show r ++ s

allCards :: [Int] -> [String] -> [Card]
allCards ranks suits = concat $ map (\rank -> map (Card rank) suits) ranks

allPerms :: (a -> b -> c) -> [a] -> [b] -> [c]
allPerms fn xs ys = concat $ map (\x -> map (fn x) ys) xs

allPairs' xs ys = allPerms (,) xs ys
allCards' xs ys = allPerms Card xs ys

allPerms3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allPerms3 fn xs ys zs = concat $ map (\x -> concat (map (\y -> map (fn x y) zs) ys)) xs
--allPerms3 fn xs ys zs = allPerms (\perm2 z -> perm2 z) (allPerms fn xs ys) zs

permStep :: [a -> b] -> [a] -> [b]
permStep fns xs = concat $ map (\x -> map (apply x) fns) xs
  where
    apply e fn = fn e

allPerms' fn xs ys = permStep (permStep [fn] xs) ys
allPerms3' fn xs ys zs = permStep (permStep (permStep fn xs) ys) zs
allPerms4' fn ws xs ys zs = permStep (permStep (permStep (permStep fn ws) xs) ys) zs
