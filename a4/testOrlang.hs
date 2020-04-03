-- How to use: runghc testFoo.hs

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import Control.Applicative

import OrlangDef
import qualified Orlang as C (interp)

-- Re-assert polymorphic type.
interp :: MonadOrlang m => Cmd -> m ()
interp = C.interp

run0 :: Cmd -> [Variables]
run0 cmd = run cmd (MkVariables 0 0)

run :: Cmd -> Variables -> [Variables]
run cmd s0 = map fst (unOM (interp cmd) s0)

testsOM =
    [ "return" ~:
      unOM (return 8) (MkVariables 3 1) ~?= [(MkVariables 3 1, 8)]

    , ">>=" ~:
      unOM (MkOM (\(MkVariables x y) -> [ (MkVariables x y, 2)
                                        , (MkVariables (x+1) y, 3)
                                        ])
            >>= \a ->
            MkOM (\(MkVariables x y) -> [ (MkVariables x y, ())
                                        , (MkVariables x (y+a), ())
                                        ]))
            (MkVariables 10 20)
      ~?= [(MkVariables 10 20, ()), (MkVariables 10 22, ()),
           (MkVariables 11 20, ()), (MkVariables 11 23, ())]
    , "empty" ~:
      unOM (empty :: OM ()) (MkVariables 1 3) ~?= []
    , "<|>" ~:
      unOM (MkOM (\(MkVariables x y) -> [ (MkVariables (x+1) y, 7)
                                        , (MkVariables x (y+1), 8)
                                        ])
            <|>
            MkOM (\(MkVariables x y) -> [ (MkVariables (x-1) y, 9)
                                        , (MkVariables x (y-1), 10)
                                        ]))
           (MkVariables 15 25)
      ~?= [(MkVariables 16 25, 7), (MkVariables 15 26, 8),
           (MkVariables 14 25, 9), (MkVariables 15 24, 10)]
    ]

-- These use OM for now. When marking I will replace with my own MonadOrlang instance.
testsOrlang =
    [ "non-determinism" ~:
      run0 (Alt (Assign X (Arith Add (Var X) (LitNum 1)))
                (Assign X (Arith Add (Var X) (LitNum 2))))
      ~?= [MkVariables 1 0, MkVariables 2 0]
    , "assert" ~:
      [ run0 (Assert (Cmp Eq (Var X) (LitNum 0)))
        ~?= [MkVariables 0 0]
      , run0 (Assert (Cmp Neq (Var X) (LitNum 0)))
        ~?= []
      ]
    , "while" ~:
      run0 (While (Cmp Lt (Var X) (LitNum 3))
             [ Assign X (Arith Add (Var X) (LitNum 1))
             , Assign Y (Arith Sub (Var Y) (LitNum 1))
             ])
      ~?= [MkVariables 3 (-3)]
    , "while and non-determinism" ~:
      run0 exampleWhileAlt
      ~?= [MkVariables 1 1, MkVariables 2 1, MkVariables 3 1, MkVariables 3 0]
    , "brute-force search" ~:
      run0 exampleBruteForce
      ~?= [MkVariables (-3) 1, MkVariables 0 1, MkVariables 2 1]
    ]

tests = testsOM ++ testsOrlang
-- more tests when marking

-- while x<3 and y==0 {
--   x:=x+1;
--   y:=1 or {};
-- }
exampleWhileAlt =
  While (Logic And (Cmp Lt (Var X) (LitNum 3)) (Cmp Eq (Var Y) (LitNum 0)))
    [ Assign X (Arith Add (Var X) (LitNum 1))
    , Alt (Assign Y (LitNum 1)) (Seq [])
    ]

-- brute-force solve x^3 + x^2 - 6x, trying x in [-4..4]
-- x := -4;
-- y := 0;
-- while x < 4 and y == 0 {
--     x := x + 1;
--     y := 1 or {};
-- }
-- assert x*(x*x + x - 6) == 0;
exampleBruteForce =
  Seq [ Assign X (LitNum (-4))
      , Assign Y (LitNum 0)
      , While (Logic And (Cmp Lt (Var X) (LitNum 4)) (Cmp Eq (Var Y) (LitNum 0)))
          [ Assign X (Arith Add (Var X) (LitNum 1))
          , Alt (Assign Y (LitNum 1)) (Seq [])
          ]
      , Assert (Cmp Eq (Arith Mul (Var X)
                                  (Arith Sub (Arith Add (Arith Mul (Var X) (Var X))
                                                        (Var X))
                                             (LitNum 6)))
                       (LitNum 0))
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
