import Test.DocTest
import Test.QuickCheck
import Test.QuickCheck.Poly

import Data.List.GroupBy

sameGroupEqual :: Blind (A -> A -> Bool) -> [A] -> Bool
sameGroupEqual (Blind p) =
    all
        (\ys ->
              and (zipWith p ys (tail ys))) .
    groupBy p

differentGroupUnequal :: Blind (A -> A -> Bool) -> [A] -> Bool
differentGroupUnequal (Blind p) xs = and (zipWith t groups (tail groups))
  where
    groups = groupBy p xs
    t ys zs = not (p (last ys) (head zs))

main :: IO ()
main = do
    quickCheck sameGroupEqual
    quickCheck differentGroupUnequal
    doctest ["-isrc", "src/"]
