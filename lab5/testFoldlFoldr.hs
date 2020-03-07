{- How to use: runghc testFoldlFoldr.hs
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import qualified FoldlFoldr as Candidate (myFoldl)

-- Re-assert desired types.
myFoldl :: (b -> a -> b) -> [a] -> (b -> b)
myFoldl = Candidate.myFoldl

tests =
    [ "myFoldl handout" ~: myFoldl (-) [1,2,3] 10 ~?= 4
      -- more test cases when marking
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
