-- How to use: runghc testRR.hs

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import qualified RR as C (rr)

-- Re-assert desired types.
rr :: Applicative f => Int -> (a -> b -> b) -> b -> f a -> f b
rr = C.rr

tests =
    [ rr 0 (-) 12 Nothing ~?= Just 12
    , rr 1 (-) 12 Nothing ~?= Nothing
    , rr 0 (-) 12 (Just 100) ~?= Just 12
    , rr 1 (-) 12 (Just 100) ~?= Just (100-12)
    , rr 2 (-) 12 (Just 100) ~?= Just (100 - (100 - 12))
    , rr 0 (-) 12 [100,200] ~?= [12]
    , rr 1 (-) 12 [100,200] ~?= [100-12, 200-12]
    , rr 2 (-) 12 [100,200] ~?= [ 100-(100-12), 100-(200-12)
                                , 200-(100-12), 200-(200-12)]
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
