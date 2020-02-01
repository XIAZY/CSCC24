module Nested where

-- [2 marks]
-- You can write nested lists in some languages such as Python, e.g.,
-- [3, [1, 4, 1], 5, [9, [2, [6], 5], 3]]

-- In Haskell, the list type alone can't do this. (Why?)
-- Your mission, should you accept it, is to make nested lists possible by
-- combining the list type with a recursive data type.

-- Define the recursive data type for one item in a nested list.
-- (Not for the whole list.  The whole list is [NestedListItem a].)

data NestedListItem a = Item a | List [NestedListItem a]
    deriving (Eq, Show)

-- such that these functions should work, for example.

flatten :: [NestedListItem a] -> [a]
flatten lst = concat (map flattenItem lst)

flattenItem :: NestedListItem a -> [a]
flattenItem (Item a) = [a]
flattenItem (List lst) = flatten lst
