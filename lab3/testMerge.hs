{-
How to use: runghc testMerge.hs
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import qualified Merge

-- Re-assert desired type.
merge :: [Integer] -> [Integer] -> [Integer]
merge = Merge.merge

tests = [ "handout" ~: merge [2, 3, 5] [1, 3, 4, 4, 7] ~?= [1, 2, 3, 3, 4, 4, 5, 7]
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
