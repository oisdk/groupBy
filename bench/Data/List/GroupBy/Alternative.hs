-- | This module provides an alternative definition of
-- 'Data.List.groupBy', written by
-- <http://brandon.si/code/an-alternative-definition-for-datalistgroupby/ Brandon Simmons>.
module Data.List.GroupBy.Alternative where

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy c (a:as) = (a : ys) : groupBy c zs
  where
    (ys,zs) = spanC a as
    spanC _ [] = ([], [])
    spanC a' (x:xs)
      | a' `c` x =
          let (ps,qs) = spanC x xs
          in (x : ps, qs)
      | otherwise = ([], x : xs)
