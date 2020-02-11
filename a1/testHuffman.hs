-- How to use: runghc testHuffman.hs

import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           Test.HUnit
import           Text.Read (readMaybe)

import qualified Data.Map.Strict as Map
import qualified Huffman as H (decode, huffmanTree, buildDict)
import           HuffmanDef

handoutTree = Branch 150 (Branch 66 (Branch 16 (Leaf 6 'd')
                                               (Leaf 10 'c'))
                                    (Leaf 50 'a'))
                         (Leaf 84 'b')

decodeTests =
  [ "decode general 1"
    ~: H.decode handoutTree [True,              -- b
                             False,True,        -- a
                             False,False,False, -- d
                             False,False,True,  -- c
                             False,True,        -- a
                             True,              -- b
                             False,False,False, -- d
                             False,True,        -- a
                             True]              -- b
    ~?= "badcabdab"
  ]

treeTests =
  [ "tree handout"
    ~: H.huffmanTree [('c', 10), ('a', 50), ('b', 84), ('d', 6)]
    ~?= handoutTree
  ]

dictTests =
  [ "dict handout"
    ~: H.buildDict handoutTree ~?= Map.fromList [('a',[False,True]),
                                                 ('b',[True]),
                                                 ('c',[False,False,True]),
                                                 ('d',[False,False,False])]
  ]

tests = decodeTests ++ treeTests ++ dictTests
-- More test cases during marking.

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
