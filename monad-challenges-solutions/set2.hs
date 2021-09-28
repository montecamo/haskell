{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude
import Set4

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay needle [] = Nothing
lookupMay needle (x:xs)
  | fst x == needle = Just $ snd x
  | otherwise = lookupMay needle xs

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just $ foldl max x xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = Just $ foldl min x xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just $ x / y 

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f Nothing = Nothing
chain f (Just a) = f a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek :: GreekData -> String -> Maybe Double
queryGreek arr needle = bind (lookupMay needle arr) (\nums -> bind (tailMay nums) (\tail -> bind (maximumMay tail) (\max -> bind (headMay nums) (\head -> divMay (fromIntegral max) (fromIntegral head)))))
-- queryGreek arr needle = link (lookupMay needle arr) (\nums -> link (tailMay nums) (\tail -> link (maximumMay tail) (\max -> link (headMay nums) (\head -> divMay (fromIntegral max) (fromIntegral head)))))

yLink :: Maybe a -> Maybe b -> (a -> b -> Maybe c) -> Maybe c
yLink Nothing _ _ = Nothing
yLink _ Nothing _ = Nothing
yLink (Just a) (Just b) f = f a b

yLink' :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
yLink' f a b = link a (\m -> link b (\z -> f m z))

mkMaybe :: a -> Maybe a
mkMaybe = Just

type Salary = (String, Integer)

addSalaries :: [Salary] -> String -> String -> Maybe Integer
addSalaries salars namea nameb = liftM2 (+) (lookupMay namea salars) (lookupMay nameb salars)
-- addSalaries salars namea nameb = link (lookupMay namea salars) (\salarya -> link (lookupMay nameb salars) (\salaryb -> mkMaybe $ salarya + salaryb))

addSalaries' :: [Salary] -> String -> String -> Maybe Integer
addSalaries' salars namea nameb = yLink (lookupMay namea salars) (lookupMay nameb salars) (\x -> mkMaybe . (+x))


transMaybe :: (a -> a -> a) -> a -> [a] -> Maybe a
transMaybe _ _ [] = Nothing
transMaybe _ elem [_] = mkMaybe elem
transMaybe f elem arr = link (tailMay arr) (\x -> mkMaybe $ foldl f elem x)


tailProd :: Num a => [a] -> Maybe a
tailProd [] = Nothing
tailProd [_] = mkMaybe 1
tailProd arr = link (tailMay arr) (\x -> mkMaybe $ foldl (*) 1 x)

tailProd' :: Num a => [a] -> Maybe a
tailProd' arr = lift (foldl (*) 1) (tailMay arr)
-- tailProd' arr = bind (tailMay arr) $ return . foldl (*) 1
-- tailProd' = transMaybe (*) 1


tailSum :: Num a => [a] -> Maybe a
tailSum [] = Nothing
tailSum [_] = mkMaybe 0
tailSum arr = link (tailMay arr) (\x -> mkMaybe $ foldl (+) 1 x)

