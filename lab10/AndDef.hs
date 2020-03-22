module AndDef where

data Expr
    = Num Integer
    | Bln Bool
    | Prim2 Op2 Expr Expr         -- Prim2 op operand operand
    -- | BoolAnd [Expr]
    -- | And [Expr]
    deriving (Eq, Show)

data Op2 = Eq | Plus | AndBool | AndFlex
    deriving (Eq, Show)

-- The type of possible values from the interpreter.
data Value = VN Integer
           | VB Bool
    deriving (Eq, Show)

-- Errors.
data Error = TypeError
    deriving (Eq, Show)
