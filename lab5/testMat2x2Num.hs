-- How to use: runghc testMat2x2Num.hs

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import Mat2x2Def
import Mat2x2Num ()

tests =
    [ "add" ~: MkMat 3 1 4 2 + MkMat 1 0 2 3 ~?= MkMat 4 1 6 5
    , "negate" ~: negate (MkMat 3 (-1) (-4) 2) ~?= MkMat (-3) 1 4 (-2)
    , "mul" ~: MkMat 3 1 4 2 * MkMat 1 0 2 3 ~?= MkMat 5 3 8 6
    , "fromInteger" ~: fromInteger 4 ~?= (MkMat 4 0 0 4 :: Mat Float)
    ]
-- More tests during marking.

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
