[![Build Status](https://travis-ci.com/oisdk/groupBy.svg?token=fXdGpZwjFQ87pr9zynKX&branch=master)](https://travis-ci.com/oisdk/groupBy)

# groupBy

This provides a drop-in replacement for Data.List.groupBy, with benchmarks and tests.


The original Data.List.groupBy has (perhaps unexpected) behaviour, in that it compares elements to the first in the group, not adjacent ones. In other words, if you wanted to group into ascending sequences:

```haskell
>>> Data.List.groupBy (<=) [1,2,2,3,1,2,0,4,5,2]
[[1,2,2,3,1,2],[0,4,5,2]]
```

Whereas this replacement performs as expected:

```haskell
>>> Data.List.GroupBy.groupBy (<=) [1,2,2,3,1,2,0,4,5,2]
[[1,2,2,3],[1,2],[0,4,5],[2]]
```

The replacement also maintains the laziness properties of the old version:

```haskell
>>> head (groupBy (==) (1:2:undefined))
[1]

>>> (head . head) (groupBy (==) (1:undefined))
1

>>> (head . head . tail) (groupBy (==) (1:2:undefined))
2
```

And is faster in almost every case, as can be seen in the performance report and benchmarks. It is also a good producer and consumer.

Also provided, for comparison, is the other implementation I could find for groupBy.

