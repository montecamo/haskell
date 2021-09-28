{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where
import Set4
import MCPrelude

data Card = Card Int String

instance Show Card where
  show (Card num t) = show num ++ t

combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep _ [] = []
combStep (f:fs) a = map f a ++ combStep fs a


allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f a = combStep $ combStep [f] a

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f a b = combStep $ combStep (map f a) b

-- allCombs4' :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
-- allCombs4' f a b c d = concat $ combStep (\x -> allCombs3' (f x) b c d) a



allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f _ [] = []
allCombs f [] _ = []
allCombs f (x:xs) arr = map (\y -> f x y) arr ++ allCombs f xs arr


allPairs :: [a] -> [b] -> [(a,b)]
allPairs = liftM2 (,)
-- allPairs = allCombs (,)

allCards :: [Int] -> [String] -> [Card]
allCards = liftM2 (Card)
-- allCards = allCombs (Card)

allCards' :: [Int] -> [String] -> [Card]
allCards' = allCombs' (Card)


-- allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
-- allCombs3 f a b c = allCombs (\x -> \y -> y x) c (allCombs f a b)
