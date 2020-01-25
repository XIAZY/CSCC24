{-
How to use: runghc testSplit.hs
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import qualified Split

-- Re-assert desired type.
split :: [a] -> ([a], [a])
split = Split.split

tests = [ "handout even" ~: split [3,1,2,9] ~?= ([3,2], [1,9])
        , "handout odd" ~: split [3,1,2,9,7] ~?= ([3,2,7], [1,9])
          -- More test cases when marking.
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
