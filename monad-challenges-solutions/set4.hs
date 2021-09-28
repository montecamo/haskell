{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where
import MCPrelude

-- generalA :: (a -> b) -> Gen a -> Gen b
-- chain :: (a -> Maybe b) -> Maybe a -> Maybe b
-- generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
-- link :: Maybe a -> (a -> Maybe b) -> Maybe b
-- yLink :: Maybe a -> Maybe b -> (a -> b -> Maybe c) -> Maybe c

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }
data Maybe a = Just {getJust :: a} | Nothing

instance Show a => Show (Maybe a) where
  -- show :: Maybe a -> String
  show (Just a) = "Justy" ++ " " ++ show a
  show Nothing = "Nothingy"

instance Eq a => Eq (Maybe a) where
  -- (==) :: Maybe a -> Maybe a -> Bool
  (Just a) == (Just b) = a == b
  Nothing == Nothing = True
  _ == _ = False

class Monad m where
  bind :: m a -> (a -> m b) -> m b
  return :: a -> m a

instance Monad Gen where
  bind a f = Gen (\seed -> let (x, seed') = runGen a seed in runGen (f x) seed')
  return x = Gen (\s -> (x, s))

instance Monad Maybe where
  bind Nothing f = Nothing
  bind (Just a) f = f a
  return = Just

instance Monad [] where
  bind arr f = concat $ map f arr
  return x = [x]

-- lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
-- lookupMay needle [] = Nothing
-- lookupMay needle (x:xs)
  -- | fst x == needle = return $ snd x
  -- | otherwise = lookupMay needle xs

-- yLink :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
-- yLink f a b = bind a (\m -> bind b (\z -> f m z))

-- type Salary = (String, Integer)
-- String -> String -> Maybe Integer
-- addSalaries salars namea nameb = yLink (\x -> (\y -> return (x + y))) (lookupMay namea salars) (lookupMay nameb salars)

lift :: Monad m => (a -> b) -> m a -> m b
lift f a = bind a $ \x -> return $ f x

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = bind ma (\a -> lift (f a) mb)

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (g:gs) = liftM2 (:) g (sequence gs) 

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f a b c = bind a $ \x -> liftM2 (f x) b c

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = bind mf $ \f -> bind ma $ return . f

join :: Monad m => m (m a) -> m a
join mma = bind mma id