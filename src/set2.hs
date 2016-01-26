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

andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen a fn = case a of
  Nothing -> Nothing
  Just x -> fn x

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 dataList searchString =
  andThen (lookupMay searchString dataList) (\xs ->
  andThen (tailMay xs) (\tailXs ->
  andThen (maximumMay tailXs) (\maxim ->
  andThen (headMay xs) (\headXs ->
    divMay (fromIntegral maxim) (fromIntegral headXs)))))

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain fn a =
  case a of
    Nothing -> Nothing
    Just x -> fn x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = andThen

mkMaybe :: a -> Maybe a
mkMaybe x = Just x

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries lst name1 name2 =
  link (find name1) (\s1 ->
  link (find name2) (\s2 ->
    mkMaybe (s1 + s2)))
  where
    find name = lookupMay name lst

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink fn mx my =
  link mx (\x ->
    link my (\y ->
        mkMaybe (fn x y)))

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 lst name1 name2 = yLink (+) (find name1) (find name2)
  where
    find name = lookupMay name lst


tailProd :: Num a => [a] -> Maybe a
tailProd lst =
  link (tailMay lst) (mkMaybe . product)

tailSum :: Num a => [a] -> Maybe a
tailSum lst = link (tailMay lst) (mkMaybe . sum)

------
generalC :: ([a] -> a) -> Maybe [a] -> Maybe a
generalC fn lst = link lst (mkMaybe . fn)

tailProd2 lst = generalC product (tailMay lst)
tailSum2 lst = generalC sum (tailMay lst)

generalTail fn lst = generalC fn (tailMay lst)

tailProd3 = generalTail product
tailSum3 = generalTail sum
-------

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe fn ma = link ma (mkMaybe . fn)

tailProd' lst = transMaybe product (tailMay lst)
tailSum' lst = transMaybe sum (tailMay lst)

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax lst = transMaybe maximumMay (tailMay lst)

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin lst = transMaybe minimumMay (tailMay lst)

combine :: Maybe (Maybe a) -> Maybe a
combine mma =
  case mma of
    Nothing -> Nothing
    Just ma -> ma

tailMax' :: Ord a => [a] -> Maybe a
tailMax' = (combine . tailMax)

tailMin' :: Ord a => [a] -> Maybe a
tailMin' = (combine . tailMin)
