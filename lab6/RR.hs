module RR where

import Control.Applicative (liftA2)

-- The intention of the "rr" function below is illustrated by:
--
-- rr 0 (-) 12 p = pure 12
-- rr 1 (-) 12 p = fmap (\x1 -> x1-12) p
--               = liftA2 (-) p (pure 12)
-- rr 2 (-) 12 p = liftA2 (\x1 x2 -> x1-(x2-12)) p p
--               = liftA3 (\x1 x2 z -> x1-(x2-z)) p p (pure 12)
--
-- The test file has some concrete examples.
--
-- Your job is to implement rr so it generalizes from 0, 1, 2 to all non-negative
-- ints, like this:
--
-- rr n (-) 12 p = liftAn (\x1 ... xn -> x1-(...-(xn-12)...)) p ... p
--                                                            (n copies of p)
--
-- Suggestion: Recall a theorem from the lecture that gives you:
--
--   liftA3 (\x1 x2 z -> x1-(x2-z)) p p (pure 12)
-- = fmap (-) p <*> (fmap (-) p <*> pure 12)
--
-- If you prefer liftA2 to <*>, it's also
--
-- = liftA2 (-) p (liftA2 (-) p (pure 12))
--
-- and perhaps it is suggesting you to let recursion do part of it!  (A similar
-- theorem holds for larger n.)

rr :: Applicative f => Int -> (a -> b -> b) -> b -> f a -> f b
rr 0 _ i _ = pure i
rr n l i p = fmap l p <*> (rr (n-1) l i p)