module SExprDef where

data SExpr = Ident String | List [SExpr] deriving (Eq, Show)
