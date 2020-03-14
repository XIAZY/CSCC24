module State where

import StateDef

{-
StateDef.hs defines the State monad and a binary tree type.  Please take a look.

You job: Working with the State monad, implement numberHelper below to convert a
binary tree to a new binary tree that has the same shape, but the elements are
Int's, and the numbers go in the order 0, 1, 2,... in the tree's in-order.
There is an example below.

You should not need to keep your own counter; use the state for the counter.
Also You should not need to write your own "MkState (\s0 -> ...)" directly; you
should only need put, get, Functor+Applicative+Monad methods, and recursion.
-}

number :: BT v -> BT Int
number t = functionize (numberHelper t) 0

numberHelper :: BT v -> State Int (BT Int)
numberHelper Null = return Null
numberHelper (Node left _ right) =
    numberHelper left
    >>= \newLeft -> get
    >>= \i -> put (i+1)
    >>= \_ -> numberHelper right
    >>= \newRight -> return (Node newLeft i newRight)

-- Example: number albertTree = albertTreeNumbered, where:

albertTree :: BT String
albertTree = Node (Node (Node Null "ll" Null)
                        "l"
                        (Node Null "lr" Null))
                  "root"
                  (Node Null
                        "r"
                        (Node Null "rr" Null))

albertTreeNumbered :: BT Int
albertTreeNumbered = Node (Node (Node Null 0 Null)
                                1
                                (Node Null 2 Null))
                          3
                          (Node Null
                                4
                                (Node Null 5 Null))
