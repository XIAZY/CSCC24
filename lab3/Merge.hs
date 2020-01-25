module Merge where

-- There are other questions in other files.

-- Take two lists of integers.  Precondition: Each is already sorted
-- (non-decreasing).  Merge them in non-decreasing order.  Linear time.
--
-- Although intended to be part of mergesort, don't make assumptions about the
-- lengths of the two lists --- they may have very different lengths.
--
-- Example:
-- merge [2, 3, 5] [1, 3, 4, 4, 7] = [1, 2, 3, 3, 4, 4, 5, 7]

merge :: [Integer] -> [Integer] -> [Integer]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys