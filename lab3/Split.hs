module Split where

-- There are other questions in other files.

-- Split a list into two, more or less equal length; intended as part of
-- mergesort.  Linear time.
--
-- Since lists are sequential, we want to "uninterleave", e.g.,
--
-- split [3,1,2,9] = ([3,2], [1,9])
-- split [3,1,2,9,7] = ([3,2,7], [1,9])
--
-- You may like to know: the tuple syntax "(foo, bar)" can be used in pattern
-- matching.

split :: [a] -> ([a], [a])
split [x] = ([x], [])
split [x, y] = ([x], [y])
split (x:y:etc) = (x:first, y:second)
    where (first, second) = split etc