module MutualClosureDef where

import Data.Map.Strict (Map)

data Expr
  = Num Integer
  | Var String
  | Prim2 Op2 Expr Expr
  | IfZero Expr Expr Expr
    -- For simplicity I don't have general if-then-else and booleans, instead an
    -- "if-zero e then e1 else e2": if e evaluates to VN 0, then e1, else e2.
  | App Expr Expr
  | LetRecFuns [(String, String, Expr)] Expr
    -- Mutually recursive function bindings and use, e.g.,
    --
    --     let f x = expr1
    --         g x = expr2
    --         h x = expr3
    --     in body
    --
    -- is represented by
    --
    --     LetRecFuns [ ("f", "x", expr1),
    --                  ("g", "x", expr2),
    --                  ("h", "x", expr3) ]
    --                body
    -- 
    -- All of expr1, expr2, expr3 may call all of f, g, h.
  deriving (Eq, Show)

data Op2 = Plus | Minus | Times deriving (Eq, Show)

-- The type of possible answers from the interpreter.
data Value = VN Integer
           | VClosure (Map String Value) String Expr
    deriving (Eq, Show)
