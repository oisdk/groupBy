module Main (main) where

import           Criterion.Main
import           System.Random

import qualified Data.List         as List
import qualified Data.List.GroupBy as GroupBy
import qualified Data.List.HT      as HT

import           Control.Monad
import           Data.Foldable

smallInt :: IO Int
smallInt = randomRIO (-3,3)

twoInt :: IO Int
twoInt = randomRIO (0,1)

length' :: [a] -> Int
length' = foldl' (\a _ -> a + 1) 0

sum' :: [Int] -> Int
sum' = foldl' (+) 0

outerLengthSmallGroups :: Int -> Benchmark
outerLengthSmallGroups n =
    env (replicateM n smallInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "Data.List" $ nf (length' . List.groupBy    (==)) xs
             , bench "GroupBy"   $ nf (length' . GroupBy.groupBy (==)) xs
             , bench "HT"        $ nf (length' . HT.groupBy      (==)) xs]

outerLengthLargeGroups :: Int -> Benchmark
outerLengthLargeGroups n =
    env (replicateM n twoInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "Data.List" $ nf (length' . List.groupBy    (==)) xs
             , bench "GroupBy"   $ nf (length' . GroupBy.groupBy (==)) xs
             , bench "HT"        $ nf (length' . HT.groupBy      (==)) xs]


outerLengthOneGroup :: Int -> Benchmark
outerLengthOneGroup n =
    env (replicateM n smallInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "Data.List" $ nf (length' . List.groupBy    (\_ _ -> True)) xs
             , bench "GroupBy"   $ nf (length' . GroupBy.groupBy (\_ _ -> True)) xs
             , bench "HT"        $ nf (length' . HT.groupBy      (\_ _ -> True)) xs]

sumSmallGroups :: Int -> Benchmark
sumSmallGroups n =
    env (replicateM n smallInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "Data.List" $ nf (sum' . map sum' . List.groupBy    (==)) xs
             , bench "GroupBy"   $ nf (sum' . map sum' . GroupBy.groupBy (==)) xs
             , bench "HT"        $ nf (sum' . map sum' . HT.groupBy      (==)) xs]

sumLargeGroups :: Int -> Benchmark
sumLargeGroups n =
    env (replicateM n twoInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "Data.List" $ nf (sum' . map sum' . List.groupBy    (==)) xs
             , bench "GroupBy"   $ nf (sum' . map sum' . GroupBy.groupBy (==)) xs
             , bench "HT"        $ nf (sum' . map sum' . HT.groupBy      (==)) xs]

sumOneGroup :: Int -> Benchmark
sumOneGroup n =
    env (replicateM n smallInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "Data.List" $ nf (sum' . map sum' . List.groupBy    (\_ _ -> True)) xs
             , bench "GroupBy"   $ nf (sum' . map sum' . GroupBy.groupBy (\_ _ -> True)) xs
             , bench "HT"        $ nf (sum' . map sum' . HT.groupBy      (\_ _ -> True)) xs]

main :: IO ()
main =
    defaultMain
        [ bgroup
              "length"
              [ bgroup "small" (map outerLengthSmallGroups [100000, 10000000])
              , bgroup "large" (map outerLengthLargeGroups [100000, 10000000])
              , bgroup "one"   (map outerLengthOneGroup    [100000, 10000000])]
        , bgroup
              "sum"
              [ bgroup "small" (map sumSmallGroups [100000, 10000000])
              , bgroup "large" (map sumLargeGroups [100000, 10000000])
              , bgroup "one"   (map sumOneGroup    [100000, 10000000])]]
