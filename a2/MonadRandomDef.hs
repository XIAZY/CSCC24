module MonadRandomDef where

class Monad m => MonadRandom m where
    choose :: Rational -> m a -> m a -> m a

data DecisionTree a = Tip a | Choose Rational (DecisionTree a) (DecisionTree a)
    deriving (Eq, Show)

data WinLose = Lose | Win
    deriving (Eq, Ord, Bounded, Enum, Show)


-- The following are for the bonus question.

data Ev a = MkEv ((a -> Rational) -> Rational)

evf :: Ev a -> (a -> Rational) -> Rational
evf (MkEv f) = f
