module Mat2x2Def where

-- Represent a 2x2 matrix, e.g.,
--     ( 3 1 )
--     ( 4 0 )
-- becomes
--     MkMat 3 1 4 0
data Mat a = MkMat a a a a
    deriving (Eq, Show)

-- In Mat2x2Num.hs, you will make "Mat a" a Num instance, provided "a" is a Num
-- instance.
