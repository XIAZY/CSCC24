module OrlangDef where

import Control.Applicative

----------------------------
-- Language constructs / AST
----------------------------

-- Two variables x, y. They hold integers.
data Var = X | Y
    deriving (Eq, Ord, Show)

data Cmd
  = Assign Var ExprNum          -- x := e
  | Seq [Cmd]                   -- { cmd; cmd; ... }, sequential compound
  | While ExprBool [Cmd]        -- while x>0 { cmd; cmd; ... }
  | Alt Cmd Cmd                 -- cmd or cmd, nondeterminism
  | Assert ExprBool             -- eg, assert x>0
  deriving (Eq, Show)

data ExprNum
  = LitNum Integer
  | Var Var
  | Neg ExprNum
  | Arith ArithOp ExprNum ExprNum
  deriving (Eq, Show)

data ArithOp = Add | Sub | Mul
    deriving (Eq, Ord, Show)

data ExprBool
  = Not ExprBool
  | Cmp Rel ExprNum ExprNum
  | Logic LogicOp ExprBool ExprBool
  deriving (Eq, Show)

data Rel = Lt | Leq | Eq | Neq
    -- less than, less than or equal to, equal, not equal
    deriving (Eq, Ord, Show)

data LogicOp = And | Or
    deriving (Eq, Ord, Show)


-----------------------------
-- Semantic model definitions
-----------------------------

-- My state has just two variables.
data Variables = MkVariables Integer Integer -- x, y
    deriving Eq

instance Show Variables where
    show (MkVariables x y) = "<x=" ++ show x ++ ", y=" ++ show y ++ ">"

-- Semantic monad.
data OM a = MkOM (Variables -> [(Variables, a)])
-- Like state monad but also [] for multiverse nondeterminism
--
-- E.g., (x:=x+1) or (x:=x+2)
-- maps <x=0,y=0> to [(<x=1,y=0>, ()), (<x=2,y=0>, ())]
--
-- E.g., assert x==0
-- maps <x=0,y=0> to [(<x=0,y=0>, ())], no state change but at least success
-- maps <x=1,y=0> to [], failure

-- Unwrap the function inside OM
unOM :: OM a -> Variables -> [(Variables, a)]
unOM (MkOM f) = f

-- Type class summarizing important semantic primitives.
-- Subclass of (includes) Monad, Alternative. Adds access to x and y.
class (Monad m, Alternative m) => MonadOrlang m where
    -- Read x or y
    get :: Var -> m Integer
    -- Write x or y
    set :: Var -> Integer -> m ()
