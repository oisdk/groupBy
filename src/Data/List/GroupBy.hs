{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module      : Data.List.GroupBy
-- Description : A replacement for 'Data.List.groupBy'
-- Copyright   : (c) Donnacha Oisín Kidney, 2018
-- License     : MIT
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides an alternative definition for
-- 'Data.List.groupBy' which does not require a transitive
-- equivalence predicate.
module Data.List.GroupBy
  (groupBy
  ,group)
  where

#if __GLASGOW_HASKELL__
import           GHC.Base (build, foldr
#if __GLASGOW_HASKELL__ >= 800
                           ,oneShot
#endif
                          )
import           Prelude  hiding (foldr)
#endif

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
-- >>> (head . head) (groupBy undefined (1:undefined))
-- 1
--
-- >>> (head . head . tail) (groupBy (==) (1:2:undefined))
-- 2
--
-- prop> xs === concat (groupBy (applyFun2 p) xs)
-- prop> all (not . null) (groupBy (applyFun2 p) xs)

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

#if __GLASGOW_HASKELL__
{-# NOINLINE [1] groupBy #-}
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
#if __GLASGOW_HASKELL__ >= 800
         oneShot
#endif
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

#else
{-# INLINE groupBy #-}
#endif

-- | Groups adjacent equal elements.
--
-- >>> group "aaabcccdda"
-- ["aaa","b","ccc","dd","a"]
group :: Eq a => [a] -> [[a]]
group = groupBy (==)
{-# INLINE group #-}

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Function
