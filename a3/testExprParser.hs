-- How to use: runghc testExprParser.hs

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import ExprDef
import ExprParser (mainParser)
import ParserLib

tests = [
  -- my custom tests
    "literal" ~:
      runParser mainParser "123"
      ~?= Just (LitNat 123)
    , "var" ~:
      runParser mainParser "abc"
      ~?= Just (Var "abc")
    , "infix" ~:
      runParser mainParser "1+2+3"
      ~?= Just (Prim2 Add (Prim2 Add (LitNat 1) (LitNat 2)) (LitNat 3))
    , "infix2" ~:
      runParser mainParser "1-2-3"
      ~?= Just (Prim2 Sub (Prim2 Sub (LitNat 1) (LitNat 2)) (LitNat 3))
    , "paren" ~:
      runParser mainParser "1+(2+3)"
      ~?= Just (Prim2 Add (LitNat 1) (Prim2 Add (LitNat 2) (LitNat 3)))
    , "eq" ~:
      runParser mainParser "1==2"
      ~?= Just (Prim2 EqNat (LitNat 1) (LitNat 2))
    , "unary" ~:
      runParser mainParser "-1"
      ~?= Just (Prim1 Neg (LitNat 1))
    , "unary 2" ~:
      runParser mainParser "! - ! 5"
      ~?= Just (Prim1 Not (Prim1 Neg (Prim1 Not (LitNat 5))))
    , "cmp" ~:
      runParser mainParser "x || y && z"
      ~?= Just (Prim2 Or (Var "x") (Prim2 And (Var "y") (Var "z")))
    -- custom tests end here
    , "|| assoc right" ~:
      runParser mainParser "x||y||z"
      ~?= Just (Prim2 Or (Var "x") (Prim2 Or (Var "y") (Var "z")))
    , "x==y==z wrong" ~:
      runParser mainParser "x==y==z"
      ~?= Nothing
    , "infix +/- assoc left and mixed" ~:
      runParser mainParser "1+2-3+4"
      ~?= Just (Prim2 Add (Prim2 Sub (Prim2 Add (LitNat 1) (LitNat 2))
                                                (LitNat 3))
                          (LitNat 4))
    , "many mixed unaries" ~:
      runParser mainParser "! - !5"
      ~?= Just (Prim1 Not (Prim1 Neg (Prim1 Not (LitNat 5))))
    , "precedence big example" ~:
      runParser mainParser "!b&&c|| -x+y==v-w && !(i==4+5||j)"
      ~?= Just (Prim2 Or
                 (Prim2 And (Prim1 Not (Var "b"))
                            (Var "c"))
                 (Prim2 And (Prim2 EqNat (Prim2 Add (Prim1 Neg (Var "x"))
                                                    (Var "y"))
                                         (Prim2 Sub (Var "v")
                                                    (Var "w")))
                            (Prim1 Not
                              (Prim2 Or (Prim2 EqNat (Var "i")
                                                     (Prim2 Add (LitNat 4) (LitNat 5)))
                                        (Var "j")))))
    ]
-- more tests when marking


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
