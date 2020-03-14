module StateDef where

data State s a = MkState (s -> (s,a))

deState :: State s a -> s -> (s, a)
deState (MkState stf) = stf

-- Like deState but furthermore discards the final state value.
functionize :: State s a -> s -> a
functionize prog s0 = snd (deState prog s0)

get :: State s s
get = MkState (\s0 -> (s0, s0))

put :: s -> State s ()
put s = MkState (\s0 -> (s, ()))

instance Monad (State s) where
    return a = pure a
    MkState t >>= k = MkState (\i0 -> case t i0 of
                                  (i1, a) -> deState (k a) i1)

instance Functor (State s) where
    fmap f (MkState t) = MkState (\i0 -> case t i0 of (i1, a) -> (i1, f a))

instance Applicative (State s) where
    pure a = MkState (\i -> (i, a))
    MkState tf <*> MkState ta = MkState
        (\i0 -> case tf i0 of (i1, f) -> case ta i1 of (i2, a) -> (i2, f a))


data BT v = Null | Node (BT v) v (BT v) deriving Eq

-- This notation goes like: (left-subtree element right-subtree).
-- Example: (((0) 1 (2)) 3 (4 (5)))
instance Show v => Show (BT v) where
    showsPrec _ t = flatsParen t
      where
        flats Null = id
        flats (Node left v right) =
            (case left of Null -> id; t -> flatsParen t . showChar ' ')
            . shows v
            . (case right of Null -> id; t -> showChar ' ' . flatsParen t)
        flatsParen t = showChar '(' . flats t . showChar ')'
