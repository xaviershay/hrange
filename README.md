hrange
======

An experimental haskell implementation of the range query language.

I am writing this to try to learn some things. My goals for Haskell include:

* Use lenses for data manipulation.
* Use a Monad stack for evaluation.
* Apply LiquidHaskell refined types where useful.
* Use STM (though I'm not really sure where this would be appropriate yet).

Range goals include:

* Allow online updating of data without requiring an expensive cache rebuild.
* Providing an efficient consistent snapshot API, where clients can run multiple queries against the same set of data.
* Provide an API for requesting multiple keys of a cluster. You can do this in
  vanilla range, but since results are returned as a set there is no way to
  programmatically map the values back to the requested keys.
* Provide an API for requesting values over a range of clusters, replacing a
  common bash loop:

    for x in $(erg -e %blah); do echo $x $(erg "$x"); done


## Architecture differences from grange

[grange](https://github.com/square/grange) represents the range data internally
very closely to the common on-disk YAML format: a cluster is a map of keys to
string values. My hypothesis is that this is expensive to traverse, since
values needs to be re-parsed and evaluated on each query. Grange works around
this by using a precomputed cache, but that takes many seconds to rebuild and
must be done for any update. For hrange, I intend to store the fully parsed and
normalized queries. I hope that this, combined with lazy evaluation, will allow
for similar performance while not requiring a cache.
