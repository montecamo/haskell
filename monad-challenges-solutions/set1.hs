{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import Set4
import MCPrelude

rand' :: Gen Integer
rand' = Gen (rand)

rands :: Int -> Gen [Integer]
rands num = sequence $ replicate num rand'

randLetter :: Gen Char
randLetter = lift toLetter rand'
--randLetter seed = let (x, seed') = rand seed in (toLetter x, seed')

randLetters :: Int -> Gen [Char]
randLetters num = sequence $ replicate num randLetter
-- randLetters num seed = let (x, seed') = randLetter seed in x : (randLetters (num - 1) seed')

-- generalA :: Gen a -> (a -> b) -> Gen b
-- generalA gen f seed = let (x, seed') = gen seed in (f x, seed')

randEven :: Gen Integer -- the output of rand * 2
randEven = lift (*2) rand'
-- randEven = generalA rand (*2)
randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = lift (+1) randEven
-- randOdd = generalA randEven (+1)
randTen :: Gen Integer -- the output of rand * 10
randTen = lift (*10) rand'
-- randTen = generalA rand (*10) 

-- generalPair :: Gen a -> Gen b -> Gen (a,b)
-- generalPair gen1 gen2 seed = 
  -- let (a, seed') = gen1 seed
      -- (b, seed'') = gen2 seed'
  -- in ((a, b), seed'')

-- genTwo :: Gen a -> (a -> Gen b) -> Gen b
-- genTwo gen f seed = let (x, seed') = gen seed in (f x) seed'

-- -- generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
-- -- generalB f gena genb seed = 
  -- -- let (a, seed') = gena seed
      -- -- (b, seed'') = genb seed'
  -- -- in (f a b, seed'')

-- generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
-- generalB f a b = genTwo a (\x -> genTwo b (\y -> mkGen (f x y)))

-- generalPair2 :: Gen a -> Gen b -> Gen (a,b)
-- generalPair2 = generalB (,)

randPair :: Gen (Char, Integer)
randPair = liftM2 (,) randLetter rand'
-- randPair = generalPair randLetter rand

-- mkGen :: a -> Gen a
-- mkGen x seed = (x, seed)

repRandom :: [Gen a] -> Gen [a]
repRandom = sequence

-- repRandom [] seed = mkGen [] seed
-- repRandom (x:xs) seed = let (a, seed') = x seed in generalA (repRandom xs) (a:) seed'

-- repRandom' :: [Gen a] -> Gen [a]
-- repRandom' = foldr (generalB (:)) $ mkGen []


