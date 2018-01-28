module Main (main) where

import           Criterion.Main
import           Criterion.Main.Options
import           Options.Applicative
import           System.IO.CodePage            (withCP65001)

import           System.Random

import qualified Data.List                     as List
import qualified Data.List.GroupBy             as GroupBy
import qualified Data.List.GroupBy.Alternative as Alt
import qualified Data.List.HT                  as HT

import           Control.Monad
import           Data.Foldable
import           Data.Semigroup                ((<>))

{-# ANN module "HLint: ignore Use group" #-}

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
             , bench "Alt"       $ nf (length' . Alt.groupBy     (==)) xs
             , bench "HT"        $ nf (length' . HT.groupBy      (==)) xs]

outerLengthLargeGroups :: Int -> Benchmark
outerLengthLargeGroups n =
    env (replicateM n twoInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "Data.List" $ nf (length' . List.groupBy    (==)) xs
             , bench "GroupBy"   $ nf (length' . GroupBy.groupBy (==)) xs
             , bench "Alt"       $ nf (length' . Alt.groupBy     (==)) xs
             , bench "HT"        $ nf (length' . HT.groupBy      (==)) xs]


outerLengthOneGroup :: Int -> Benchmark
outerLengthOneGroup n =
    env (replicateM n smallInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "Data.List" $ nf (length' . List.groupBy    (\_ _ -> True)) xs
             , bench "GroupBy"   $ nf (length' . GroupBy.groupBy (\_ _ -> True)) xs
             , bench "Alt"       $ nf (length' . Alt.groupBy     (\_ _ -> True)) xs
             , bench "HT"        $ nf (length' . HT.groupBy      (\_ _ -> True)) xs]

sumSmallGroups :: Int -> Benchmark
sumSmallGroups n =
    env (replicateM n smallInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "Data.List" $ nf (sum' . map sum' . List.groupBy    (==)) xs
             , bench "GroupBy"   $ nf (sum' . map sum' . GroupBy.groupBy (==)) xs
             , bench "Alt"       $ nf (sum' . map sum' . Alt.groupBy     (==)) xs
             , bench "HT"        $ nf (sum' . map sum' . HT.groupBy      (==)) xs]

sumLargeGroups :: Int -> Benchmark
sumLargeGroups n =
    env (replicateM n twoInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "Data.List" $ nf (sum' . map sum' . List.groupBy    (==)) xs
             , bench "GroupBy"   $ nf (sum' . map sum' . GroupBy.groupBy (==)) xs
             , bench "Alt"       $ nf (sum' . map sum' . Alt.groupBy     (==)) xs
             , bench "HT"        $ nf (sum' . map sum' . HT.groupBy      (==)) xs]

sumOneGroup :: Int -> Benchmark
sumOneGroup n =
    env (replicateM n smallInt) $
    \xs ->
         bgroup
             (show n)
             [ bench "Data.List" $ nf (sum' . map sum' . List.groupBy    (\_ _ -> True)) xs
             , bench "GroupBy"   $ nf (sum' . map sum' . GroupBy.groupBy (\_ _ -> True)) xs
             , bench "Alt"       $ nf (sum' . map sum' . Alt.groupBy     (\_ _ -> True)) xs
             , bench "HT"        $ nf (sum' . map sum' . HT.groupBy      (\_ _ -> True)) xs]

defaultSizes :: [Int]
defaultSizes = [10000]

sizesParser :: Parser [Int]
sizesParser =
    option
        auto
        (long "sizes" <> help "a list of the sizes on which to run groupBy" <>
         value defaultSizes)

overParser :: (Parser a -> Parser b) -> ParserInfo a -> ParserInfo b
overParser f p =
    p
    { infoParser = f (infoParser p)
    }


main :: IO ()
main =
    withCP65001 $
    do (sizes,wat) <-
           execParser
               (overParser (liftA2 (,) sizesParser) (describe defaultConfig))
       runMode
           wat
           [ bgroup
                 "length"
                 [ bgroup "small" (map outerLengthSmallGroups sizes)
                 , bgroup "large" (map outerLengthLargeGroups sizes)
                 , bgroup "one" (map outerLengthOneGroup sizes)]
           , bgroup
                 "sum"
                 [ bgroup "small" (map sumSmallGroups sizes)
                 , bgroup "large" (map sumLargeGroups sizes)
                 , bgroup "one" (map sumOneGroup sizes)]]
