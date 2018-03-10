hrange
======

An experimental haskell implementation of the range query language.

I am writing this to try to learn some things. My goals for Haskell include:

* **[DONE]** Use lenses for data manipulation.
* **[DONE]** Use a Monad stack for evaluation.
* **[N/A]** Apply LiquidHaskell refined types where useful.
* **[N/A]** Use STM (though I'm not really sure where this would be appropriate
  yet).

Range goals include:

* Allow online updating of data without requiring an expensive cache rebuild.
* Providing an efficient consistent snapshot API, where clients can run
  multiple queries against the same set of data.
* Provide an API for requesting multiple keys of a cluster. You can do this in
  vanilla range, but since results are returned as a set there is no way to
  programmatically map the values back to the requested keys.
* Provide an API for requesting values over a range of clusters, replacing a
  common bash loop:

        for x in $(erg -e %blah); do echo $x $(erg "$x"); done

Current status: parked. It's a fully working range implementation, but I
never got around to any of the bonus goals listed above.

## Usage

This package includes three components:

* The core [`HRange` library](src/Hrange.hs), with functions for evaluating
  range queries.
* A proof-of-concept [web server](web server) that reads state from YAML files
  and responds to online queries against that state via HTTP.
* A CLI tool [`ergh`](cli/Main.hs) for debugging directly against range state
  as defined in YAML files.

The last two components can be run with:

    stack build
    stack exec hrange-server
    stack exec ergh

## Documentation

Usage documentation is written with haddock. Unfortunately this isn't hosted
anywhere, but you can generate it locally or just read it in the source files.

    stack haddock

## Development

    git clone https://github.com/xaviershay/hrange.git
    cd hrange
    git submodule init
    git submodule update

    stack test                                      # Run all tests
    stack test --test-arguments '-p "name of spec"' # Run individual test

The majority of the tests are defined by the
[range-spec](https://github.com/square/range-spec) project, which is included
as a submodule.

## Architecture differences from grange

[grange](https://github.com/square/grange) represents the range data internally
very closely to the common on-disk YAML format: a cluster is a map of keys to
string values. My hypothesis is that this is expensive to traverse, since
values needs to be re-parsed and evaluated on each query. Grange works around
this by using a precomputed cache, but that takes many seconds to rebuild and
must be done for any update. For hrange, I intend to store the fully parsed and
normalized queries. I hope that this, combined with lazy evaluation, will allow
for similar performance while not requiring a cache. **Update: nope, cache
still necessary. AST is much nicer to work with though.**
