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

length' :: [a] -> Int
length' = foldl' (\a _ -> a + 1) 0

outerLengthSmallGroups :: Int -> Benchmark
outerLengthSmallGroups n =
    env (replicateM n smallInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "Data.List" $ nf (length' . List.groupBy (==)) xs
             , bench "GroupBy" $ nf (length' . GroupBy.groupBy (==)) xs
             , bench "HT" $ nf (length' . HT.groupBy (==)) xs]


main :: IO ()
main =
    defaultMain
        [ bgroup
              "Outer length, small groups"
              (map outerLengthSmallGroups [100000, 10000000])]
