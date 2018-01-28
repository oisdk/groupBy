-- | This module provides an alternative definition for
-- 'Data.List.groupBy' which does not require a transitive
-- equivalence predicate.
module Data.List.GroupBy where

import           GHC.Base (build, foldr, oneShot)
import           Prelude  hiding (foldr)

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

{-# NOINLINE [1] groupBy #-}
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy p' (x':xs') = (x' : ys') : zs'
  where
    (ys',zs') = go p' x' xs'
    go p z (x:xs)
      | p z x = (x : ys, zs)
      | otherwise = ([], (x : ys) : zs)
      where (ys,zs) = go p x xs
    go _ _ [] = ([], [])

{-# INLINE [0] groupByFB #-}
{-# ANN groupByFB "hlint: ignore Redundant lambda" #-}
groupByFB
    :: (a -> a -> Bool)
    -> ([a] -> b -> b)
    -> a
    -> ((a -> Bool) -> ([a], b))
    -> (a -> Bool)
    -> ([a], b)
groupByFB p c =
    \x a ->
         oneShot
             (\q ->
                   let (ys,zs) = a (p x)
                   in if q x
                          then (x : ys, zs)
                          else ([], c (x : ys) zs))


{-# INLINE [0] constGroupBy #-}
constGroupBy :: a -> b -> ([c], a)
constGroupBy n = const ([], n)

{-# INLINE [0] constFalse #-}
constFalse :: a -> Bool
constFalse = const False

{-# INLINE [0] sndGroupBy #-}
sndGroupBy :: (a, b) -> b
sndGroupBy = snd

{-# RULES
"groupBy"     [~1] forall p xs. groupBy p xs = build (\c n -> sndGroupBy (foldr (groupByFB p c) (constGroupBy n) xs constFalse))
"groupByList" [1]  forall p xs. sndGroupBy (foldr (groupByFB p (:)) (constGroupBy []) xs constFalse) = groupBy p xs #-}

-- | Groups adjacent equal elements.
--
-- >>> group "aaabcccdda"
-- ["aaa","b","ccc","dd","a"]
group :: Eq a => [a] -> [[a]]
group = groupBy (==)
{-# INLINE group #-}
