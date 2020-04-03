-- How to use: runghc testMutualClosure.hs

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import MutualClosureDef
import MutualClosure (mainInterp)

tests = [ "sample" ~:
          mainInterp (LetRecFuns fgdef (App (Var "f") (Num 11)))
          ~?= Right (VN 32)
        ]
-- more tests when marking

-- f n = if n==0 then 0 else f (n-1) + g (n-1)
-- g n = if n==0 then 1 else g (n-1) - f (n-1)
fgdef = [ ("f", "n",
           (IfZero (Var "n")
             (Num 0)
             (Prim2 Plus
               (App (Var "f") (Prim2 Minus (Var "n") (Num 1)))
               (App (Var "g") (Prim2 Minus (Var "n") (Num 1))))))
        , ("g", "n",
           (IfZero (Var "n")
             (Num 1)
             (Prim2 Minus
               (App (Var "g") (Prim2 Minus (Var "n") (Num 1)))
               (App (Var "f") (Prim2 Minus (Var "n") (Num 1))))))
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
