{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ show a


headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay needle ((a,b):xs)
  | a == needle = Just b
  | otherwise = lookupMay needle xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay a 0 = Nothing
divMay a b = Just $ a / b

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (m:[]) = Just m
maximumMay (m:n:xs)
  | m > n = maximumMay (m:xs)
  | otherwise = maximumMay (n:xs)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (m:[]) = Just m
minimumMay (m:n:xs)
  | m < n = minimumMay (m:xs)
  | otherwise = minimumMay (n:xs)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek dataList searchString =
  case lookupMay searchString dataList of
    Nothing -> Nothing
    Just xs -> case tailMay xs of
      Nothing -> Nothing
      Just tailXs -> case maximumMay tailXs of
        Nothing -> Nothing
        Just maxim -> case headMay xs of
          Nothing -> Nothing
          Just headXs -> divMay (fromIntegral maxim) (fromIntegral headXs)
