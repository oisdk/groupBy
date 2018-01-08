module Data.List.GroupBy where

import GHC.Base (build)

-- $setup
-- >>> import Test.QuickCheck

-- | Groups adjacent elements according to some relation.
-- The relation can be an equivalence:
--
--
-- >>> groupBy (==) "aaabcccdda"
-- ["aaa","b","ccc","dd","a"]
--
-- >>> groupBy (==) []
-- []
--
-- However, it need not be. The function compares adjacent
-- elements only, so it can be used to find increasing
-- subsequences:
--
-- >>> groupBy (<=) [1,2,2,3,1,2,0,4,5,2]
-- [[1,2,2,3],[1,2],[0,4,5],[2]]
--
-- It is fully lazy:
--
-- >>> head (groupBy (==) (1:2:undefined))
-- [1]
--
-- >>> (head . head) (groupBy (==) (1:undefined))
-- 1
--
-- >>> (head . head . tail) (groupBy (==) (1:2:undefined))
-- 2
--
-- prop> xs === concat (groupBy (getBlind p) xs)
-- prop> all (not . null) (groupBy (getBlind p) xs)
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy p xs = build (\c n ->
  let f x a q
        | q x = (x : ys, zs)
        | otherwise = ([], c (x : ys) zs)
        where (ys,zs) = a (p x)
  in snd (foldr f (const ([], n)) xs (const False)))
{-# INLINE groupBy #-}
