module ExprDef where

data Expr = LitNat Integer
          | Var String
          | Prim1 Op1 Expr
          | Prim2 Op2 Expr Expr
    deriving (Eq, Show)

data Op1 = Not | Neg
    deriving (Eq, Show)

data Op2 = Add | Sub | EqNat | And | Or
    deriving (Eq, Show)

