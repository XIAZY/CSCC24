module MutualClosure where

-- See the end for the question.

import MutualClosureDef

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

raise :: String -> Either String a
raise = Left

mainInterp :: Expr -> Either String Value
mainInterp expr = interp expr Map.empty

intOrDie (VN i) = pure i
intOrDie _ = raise "type error"

interp :: Expr -> Map String Value -> Either String Value
interp (Num i) env = pure (VN i)
interp (Var v) env = case Map.lookup v env of
  Just val -> pure val
  Nothing -> raise (v ++ " not found")
interp (Prim2 op e1 e2) env =
    interp e1 env
    >>= \a -> intOrDie a
    >>= \i -> interp e2 env
    >>= \b -> intOrDie b
    >>= \j -> pure (VN (binop op i j))
  where
    binop Plus = (+)
    binop Minus = (-)
    binop Times = (*)
interp (IfZero e e0 e1) env =
    interp e env
    >>= \val -> case val of
      VN 0 -> interp e0 env
      _ -> interp e1 env
interp (App f e) env =
    interp f env
    >>= \c -> case c of
      VClosure fEnv v body ->
          interp e env
          >>= \eVal -> let bEnv = Map.insert v eVal fEnv
                       in interp body bEnv
      _ -> raise "wrong type in App"

interp (LetRecFuns defs evalMe) env =
  interp evalMe env' where
    env' = getenv defs env
    getenv [] e = e
    getenv [(f,v,fbody)] e = Map.insert f (VClosure env' v fbody) e
    getenv (x:xs) e= getenv xs (getenv [x] e)

-- LetRecFuns is [mutually] recursive function bindings and use, e.g.,
--
--     let f x = expr1
--         g x = expr2
--         h x = expr3
--     in eval-me
--
-- is represented as
--
--     LetRecFuns [ ("f", "x", expr1),
--                  ("g", "x", expr2),
--                  ("h", "x", expr3) ]
--                eval-me
--
-- All of expr1, expr2, expr3 may call all of f, g, h.  An example is in the
-- sample test case.
--
-- Complete the LetRecFuns case of interp to support a group of arbitrarily many
-- self- and/or mutual-recursive functions.  See mutual-clousre.jpg for a hint
-- or visualized objective.
