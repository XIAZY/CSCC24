module LeftistHeap(PQueue, empty, insert, extractMin, merge) where

-- Priority queue type. Type parameters in order:
--   pri: priority type
--   job: job type
data PQueue pri job
  = E
  | N Int pri job (PQueue pri job) (PQueue pri job)

-- The empty priority queue.
empty :: PQueue pri job
empty = E

-- Add a job and its priority to a priority queue, get a new priority queue.
-- Argument order: priority, job, queue.
insert :: Ord pri => pri -> job -> PQueue pri job -> PQueue pri job
insert p j h = merge (N 1 p j E E) h

-- Take out a job of lowest priority from a priority queue.
-- If the queue is empty, get Nothing.
-- If the queue is non-empty, get Just (new queue, chosen job).
extractMin :: Ord pri => PQueue pri job -> Maybe (PQueue pri job, job)
extractMin E = Nothing
extractMin (N r p j lt rt) = Just (merge lt rt, j)

-- Merge two priority queues.
merge :: Ord pri => PQueue pri job -> PQueue pri job -> PQueue pri job
merge E h2 = h2
merge h1 E = h1
merge h1@(N _ p1 j1 lt1 rt1) h2@(N _ p2 j2 lt2 rt2)
    | p1 <= p2 = mkNode p1 j1 lt1 (merge rt1 h2)
    | otherwise = mkNode p2 j2 lt2 (merge rt2 h1)


-- Internal helper functions below.

getRank E = 0
getRank (N r _ _ _ _) = r

mkNode :: pri -> job -> PQueue pri job -> PQueue pri job -> PQueue pri job
mkNode p j t1 t2
    | r1 >= r2 = N (1 + r2) p j t1 t2
    | otherwise = N (1 + r1) p j t2 t1
  where
    r1 = getRank t1
    r2 = getRank t2
