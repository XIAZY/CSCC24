module MonadRandom where

import Control.Applicative (liftA2)

import MonadRandomDef

import Data.List

instance Functor DecisionTree where
    -- If you are confident in your Monad instance, you may use:
    fmap f t = t >>= \x -> return (f x)

instance Applicative DecisionTree where
    -- If you are confident in your Monad instance, you may use:
    pure a = Tip a
    liftA2 op t1 t2 = t1 >>= \x1 -> t2 >>= \x2 -> return (op x1 x2)

instance Monad DecisionTree where
    return a = Tip a
    (Tip a) >>= f = f a
    (Choose p l r) >>= f = (Choose p (l >>= f) (r >>= f))
    
instance MonadRandom DecisionTree where
    choose p l r = Choose p l r

expectedValue :: Fractional q => (t -> q) -> DecisionTree t -> q
expectedValue rv (Tip a) = rv a
expectedValue rv (Choose p l r) = evl + evr where
  evl = (expectedValue rv l) * fromRational p
  evr = (expectedValue rv r) * fromRational (1 - p)

probability :: Fractional q => (t -> Bool) -> DecisionTree t -> q
probability pred t = expectedValue bernoulli t
  where
    bernoulli a | pred a = 1
                | otherwise = 0

uniform :: MonadRandom m => [a] -> m a
uniform [x] = return x
uniform xs = choose ((fromIntegral hs) / (fromIntegral s)) l r where
    s = length xs
    hs = div s 2
    l = uniform (take hs xs)
    r = uniform (drop hs xs)

hangman :: MonadRandom m => String -> Int -> String -> m WinLose
hangman w t s = hangmanHelper (length (nub w)) t (length s)

hangmanHelper :: MonadRandom m => Int -> Int -> Int -> m WinLose
hangmanHelper 0 _ _ = return Win
hangmanHelper _ 0 _ = return Lose
hangmanHelper w t s = choose p l (return Lose) where
  p = (fromIntegral w) / (fromIntegral s)
  l = hangmanHelper (w-1) (t-1) (s-1)