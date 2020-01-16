{-
How to use:

* All tests: runghc testLabw02.hs

* Individual test e.g. 2nd: runghc testLabw02.hs 1
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import Labw02 (pow)

tests = [ "0^0" ~: pow 0 0 ~?= 1
        , "7^0" ~: pow 7 0 ~?= 1
        , "(-3)^43" ~: pow (-3) 43 ~?= -328256967394537077627
        -- more tests during marking
        ]

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
