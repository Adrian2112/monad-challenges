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
