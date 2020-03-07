module FilterFoldr where

{- "filter pred lst" picks out the list items that satisfy the predicate. It can
   be implemented as:

   filter pred [] = []
   filter pred (x:xs) = if pred x then x : filter pred xs
                                  else filter pred xs

   E.g., filter (\x -> x > 0) [1, -2, 3] = [1, 3]

   filter is in the standard library, if you want to try it.

   Write a new version that uses foldr to replace the recursion.

   Note: Please stick the the given format. More details below.

   Note 2: The list parameter is deliberately absent. This is legal. Also you
   absolutely don't need to use it directly.
-}

-- Please don't change the following 3 lines.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr op z
  where
-- Please don't change the 3 lines above.
-- Below, changing the format is OK, e.g., "op x r = ...", or using guards.
    z = []
    op x r = if pred x then x : r else r
