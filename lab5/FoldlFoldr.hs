module FoldlFoldr where

{- foldl can be expressed as foldr too! It is difficult, but it's less difficult
   if we change the parameter order to put the "accumulator" as the last
   parameter.

   E.g., newFoldl (-) [1,2,3] 10 = ((10-1)-2)-3 = 4

   Here is the actual code of newFoldl. To reduce name clashes, I'm renaming
   "op" to "f", "z" to "b".
-}
newFoldl :: (b -> a -> b) -> [a] -> b -> b
newFoldl f [] b = b
newFoldl f (x : xs) b = newFoldl f xs (f b x)

{- Below, fill in the new version that uses foldr.

   Note: Please stick to the given format. More details below.

   Note 2: Both the list parameter and the accumulator parameter are
   deliberately absent!  You absolutely don't need them.

   Hint: "newFoldl f xs" and "myFoldl f xs" produce a "b->b" function. Aim for
   that.
-}

-- Please don't change the following 3 lines.
myFoldl :: (b -> a -> b) -> [a] -> (b -> b)
myFoldl f = foldr op z
  where
-- Please don't change the 3 lines above.
-- Below, changing the format is OK, e.g., "op x r = ...".
    z = \b -> b
    op x r = \b -> r (f b x)


{- How to solve it:

   As hinted, first rewrite newFoldl to produce a "b->b" function:

   newFoldl f [] = \b -> b
   newFoldl f (x:xs) = \b -> newFoldl f xs (f b x)
                     = \b -> (newFoldl f xs) (f b x)

   So z = \b -> b,
      op x r = \b -> r (f b x)

   If this is still too hard too see, pretend induction may help:

   WTP: for all lst. newFoldl f lst = foldr op z lst
   and find z and op.

   Base case: lst = []

     LHS = \b -> b
     RHS = foldr op z []
         = z

     Define z = \b -> b

     Now LHS = RHS.

   Induction step: lst = x:xs
   Induction hypothesis:
     newFold f xs = foldr op (\b -> b) xs
   WTP:
     newFold f (x:xs) = foldr op (\b -> b) (x:xs)

       LHS
     = \b -> newFoldl f xs (f b x)     by newFold code

       RHS
     = op x (foldr op (\b -> b) xs)    by foldr code
     = op x (newFoldl f xs)            by I.H.

     We need op to satisfy
        op x (newFoldl f xs) = \b -> newFoldl f xs (f b x)
     Generalize from "(newFoldl f xs)" to arbitrary r:
        op x r = \b -> r (f b x)

     Now LHS = RHS.
-}
