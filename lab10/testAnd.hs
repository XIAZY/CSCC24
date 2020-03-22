-- How to use: runghc testAnd.hs

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import AndDef
import And (interp)

tests =
    [ "AndBool left identity"
      ~: interp (Prim2 AndBool (Bln False) (Num 4))
      ~?= Right (VB False)
    , "AndBool right"
      ~: interp (Prim2 AndBool (Bln True) (Bln False))
      ~?= Right (VB False)
    , "AndBool type error"
      ~: interp (Prim2 AndBool (Num 4) (Bln True))
      ~?= Left TypeError
    , "AndFlex left identity"
      ~: interp (Prim2 AndFlex (Bln False) (Num 4))
      ~?= Right (VB False)
    , "AndFlex right"
      ~: interp (Prim2 AndFlex (Num 4) (Num 10))
      ~?= Right (VN 10)
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
