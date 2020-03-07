{-
How to use: runghc testFilterFoldr.hs
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import qualified FilterFoldr as F (myFilter)

-- Re-assert desired type.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = F.myFilter

tests =
    [ TestCase (assertBool
                "myFilter (\\_ -> True) []"
                (null (myFilter (\_ -> True) [])))
    , "myFilter handout" ~: myFilter (\x -> x > 0) [1, -2, 3] ~?= [1, 3]
    -- more tests when marking
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
