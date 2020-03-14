{-
How to use:

    runghc testState.hs
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import StateDef
import State (numberHelper)

number :: BT v -> BT Int
number t = functionize (numberHelper t) 0

tests = [ "Albert's tree" ~: number albertTree ~?= albertTreeNumbered
        -- more tests when marking
        ]

albertTree = Node (Node (Node Null "ll" Null)
                        "l"
                        (Node Null "lr" Null))
                  "root"
                  (Node Null
                        "r"
                        (Node Null "rr" Null))
albertTreeNumbered = Node (Node (Node Null 0 Null)
                                1
                                (Node Null 2 Null))
                          3
                          (Node Null
                                4
                                (Node Null 5 Null))

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
