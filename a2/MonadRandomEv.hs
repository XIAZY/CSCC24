module MonadRandomEv where

import MonadRandomDef

instance Functor Ev where

instance Applicative Ev where

instance Monad Ev where

instance MonadRandom Ev where
