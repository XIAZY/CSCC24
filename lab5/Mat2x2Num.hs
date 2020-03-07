module Mat2x2Num where

import Mat2x2Def

instance Num a => Num (Mat a) where
    -- (+) :: Mat a -> Mat a -> Mat a
    -- matrix addition
    MkMat a b c d + MkMat a' b' c' d' = MkMat (a+a') (b+b') (c+c') (d+d')

    -- negate :: Mat a -> Mat a
    -- negate every element
    negate (MkMat a b c d) = MkMat (negate a) (negate b) (negate c) (negate d)

    -- No need to do (-), and not tested during marking, default implementation
    -- uses your (+) and negate.

    -- (*) :: Mat a -> Mat a -> Mat a
    -- matrix multiplication
    MkMat a11 a12 a21 a22 * MkMat b11 b12 b21 b22 = MkMat c11 c12 c21 c22
      where
        c11 = a11*b11 + a12*b21
        c12 = a11*b12 + a12*b22
        c21 = a21*b11 + a22*b21
        c22 = a21*b12 + a22*b22

    -- We skip abs and signum this time.
    abs = error "not required"
    signum = error "not required"

    -- fromInteger :: Integer -> Mat a
    -- Conceptually, and by example,
    --
    -- fromInteger 4 = ( 4 0 )
    --                 ( 0 4 )
    --
    -- but watch the types!  The LHS 4 has type Integer, but the RHS 4s need to
    -- have type "a".  How do you convert?  Hint: "a" itself is a Num instance...
    fromInteger a = MkMat (fromInteger a) 0 0 (fromInteger a)
