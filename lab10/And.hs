-- In this lab exercise, you try your hands at formulating a short-circuiting
-- operator: logical-and.
--
-- There are two versions below: AndBool, AndFlex.  In short, AndBool insists on
-- the boolean type, but AndFlex is much more flexible about types; see the
-- comments below for details.  (Dynamically typed languages such as Scheme tend
-- to prefer the AndFlex way.)
--
-- For simplicity there are no variables, environments, or functions to worry
-- about, just a bit of arithmetic.

module And where

import AndDef

-- Helper to raise errors.
raise :: Error -> Either Error a
raise = Left

-- Helper to expect the VN case (failure if not) and return the integer.
intOrDie :: Value -> Either Error Integer
intOrDie (VN i) = pure i
intOrDie _ = raise TypeError

-- Helper to expect the VN case (failure if not) and return the integer.
boolOrDie :: Value -> Either Error Bool
boolOrDie (VB b) = pure b
boolOrDie _ = raise TypeError

interp :: Expr -> Either Error Value

interp (Num i) = pure (VN i)

interp (Bln b) = pure (VB b)

interp (Prim2 Plus e1 e2) =
    interp e1
    >>= \a -> intOrDie a
    >>= \i -> interp e2
    >>= \b -> intOrDie b
    >>= \j -> return (VN (i+j))

interp (Prim2 Eq e1 e2) =
    interp e1
    >>= \a -> intOrDie a
    >>= \i -> interp e2
    >>= \b -> intOrDie b
    >>= \j -> return (VB (i == j))

-- Short-circuiting boolean logical-and.  If the 1st operand evaluates to false,
-- don't evaluate the 2nd operand.  In any case, every evaluated operand must be
-- checked to give a boolean value.
interp (Prim2 AndBool e1 e2) = 
    interp e1
    >>= \a -> boolOrDie a
    >>= \i -> case i of 
        False -> return (VB False)
        _ -> interp e2
            >>= (\b -> boolOrDie b)
            >>= \j -> return (VB j)

-- Short-circuiting logical-and, but flexible in types.  If the 1st operand
-- evaluates to boolean false, don't evaluate the 2nd operand; else, proceed to
-- evaluate the second operand and use its result as the overall result.  There
-- is no restriction on types.
interp (Prim2 AndFlex e1 e2) =
    case l of
        Right (VB False) -> return (VB False)
        _ -> interp e2
    where l = interp e1