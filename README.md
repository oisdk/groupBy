[![Build Status](https://travis-ci.org/oisdk/groupBy.svg?branch=master)](https://travis-ci.org/oisdk/groupBy)

# groupBy

This provides a drop-in replacement for [`Data.List.groupBy`](https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-List.html#v:groupBy), with benchmarks and tests.

The original `Data.List.groupBy` has (perhaps unexpected) behaviour, in that it compares elements to the first in the group, not adjacent ones. In other words, if you wanted to group into ascending sequences:

```haskell
>>> Data.List.groupBy (<=) [1,2,2,3,1,2,0,4,5,2]
[[1,2,2,3,1,2],[0,4,5,2]]
```

The replacement has three distinct advantages:

1. It groups adjacent elements, allowing the example above to function as expected:

   ```haskell
   >>> Data.List.GroupBy.groupBy (<=) [1,2,2,3,1,2,0,4,5,2]
   [[1,2,2,3],[1,2],[0,4,5],[2]]
   ```

2. It is a good producer and consumer, with rules similar to those for [`Data.List.scanl`](https://hackage.haskell.org/package/base-4.10.1.0/docs/src/GHC.List.html#scanl). The old version was defined in terms of [`span`](https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-List.html#v:span):

   ```haskell
   groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
   groupBy _  []           =  []
   groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
                              where (ys,zs) = span (eq x) xs
   ```
   
   Which prevents it from being a good producer/consumer.

3. It is significantly faster than the original in most cases.
   
## Tests

Tests ensure that the function is the same as the original when the relation supplied is an equivalence, and that it performs the expected adjacent comparisons when the relation isn't transitive.

The tests also check that laziness is maintained, as defined by:

```haskell
>>> head (groupBy (==) (1:2:undefined))
[1]

>>> (head . head) (groupBy undefined (1:undefined))
1

>>> (head . head . tail) (groupBy (==) (1:2:undefined))
2
```

## Benchmarks

Benchmarks compare the function to three other implementations: the current [`Data.List.groupBy`](https://hackage.haskell.org/package/base-4.10.1.0/docs/src/Data.OldList.html#groupBy), a [version](https://hackage.haskell.org/package/utility-ht-0.0.14/docs/Data-List-HT.html#v:groupBy) provided by the [utility-ht](https://hackage.haskell.org/package/utility-ht) package, and [a version provided by Brandon Simmons](http://brandon.si/code/an-alternative-definition-for-datalistgroupby/).

The benchmarks test functions that force the outer list:

```haskell
length . groupBy eq
```

And functions which force the contents of the inner lists:

```haskell
sum' = foldl' (+) 0

sum' . map sum' . groupBy eq
```

Each benchmark is run on lists where the groups are small, the groups are large, and where there is only one group. The default size is 10000, but other sizes can be provided with the `--size=[x,y,z]` flag to the benchmarks.

The new definition is slower than the old only when the size of the sublists is much larger than the size of the outer list. To make the newer definition faster in *that* case, you would simply force the pair (or use a strict pair) from the accumulator. However, this makes the new definition match the old speed in the other cases, which I would imagine are more common.
