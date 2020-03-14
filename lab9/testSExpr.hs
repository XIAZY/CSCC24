{-
How to use:

    runghc testSExpr.hs
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import SExpr (sexpr)
import SExprDef
import ParserLib

tests = [ "example" ~:
          runParser sexpr "(  f  ( g  x1 y1)  (h))  "
          ~?= Just (List [ Ident "f"
                         , List [Ident "g",Ident "x1",Ident "y1"]
                         , List [Ident "h"]])
        , "leftover" ~:
          unParser sexpr "x1 y "
          ~?= Just ("y ", Ident "x1")
        ]
-- More tests when marking.

main = do
    args <- getArgs
    case args of
      a:_ | Just n <- readMaybe a, 0 <= n, n < length tests ->
            do c@Counts{errors=e, failures=f} <- runTestTT (tests !! n)
               if e == 0 && f == 0
                   then return c
                   else exitFailure
          | otherwise -> error "No such test number."
      _ -> runTestTT (TestList tests)
