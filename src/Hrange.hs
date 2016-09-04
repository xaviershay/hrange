-- |An experimental haskell implementation of the range query language. It is
-- an expressive grammar for selecting information out of arbitrary,
-- self-referential metadata. It was developed for querying information about
-- hosts across datacenters.
--
-- /This is an incomplete work-in-progress that may never be completed. Do not use./
--
-- The author of this library also authored
-- <https://godoc.org/github.com/square/grange grange>, which explains any
-- suspicious similarities between the two.
--
-- = Basics
--
-- A range query operates on a state containing clusters.
--
-- >>> state = addCluster "a" [("CLUSTER", ["a", "b", "c"]), ("TYPE", ["letters"])] emptyState
-- >>> expand state "%a"
-- makeResult ["a", "b", "c"]
-- >>> expand state "%a:KEYS"
-- makeResult ["CLUSTER", "TYPE"]
-- >>> expand state "%a:TYPE"
-- makeResult ["letters"]
--
-- Range also allows for a default cluster (named @GROUPS@), that can be
-- accessed with some shortcut syntax documented below.
--
-- Values can also be range expressions, so that clusters can be defined in
-- terms of each other ("self-referential").
--
-- >>> state = addCluster "down" [("CLUSTER", ["host1"])] $ addCluster "dc1" [("CLUSTER", ["@dc1 - %down"])] $ emptyState
-- >>> expand state "%dc1"
-- makeResult ["host2"]
--
-- = Syntax
--
-- > host1         - value constant, returns itself.
-- > host1,host2   - union, concatenates both sides.
-- > host1..3      - numeric expansion.
-- > a{b,c}d       - brace expansion, works just like your shell.
-- > (a,b) & a     - returns intersection of boths sides.
-- > (a,b) - a     - returns left side minus right side.
-- > /abc/         - regex match using RE2 semantics. When used on the right
-- >                 side of an operator, filters the left side values using the
-- >                 regex.  When used by itself, matches all values in the
-- >                 default cluster..
-- > %dc1          - cluster lookup, returns the values at CLUSTER key in "dc1"
-- >                 cluster.
-- > %dc1:KEYS     - returns all available keys for a cluster.
-- > %dc1:SOMEKEY  - returns values at SOMEKEY key.
-- > %dc1:{A,B}    - returns values at both A and B key. Query inside braces can
-- >                 be any range expression.
-- > @dc1          - key lookup in default cluster, equivalent to %GROUPS:dc1.
-- > $SOMEKEY      - Looks up values from SOMEKEY in the current cluster when
-- >                 used as a cluster value. When used at top-level, the
-- >                 default cluster is used.
-- > ?host1        - returns all keys in the default cluster that contain host1.
-- > clusters(h1)  - returns all clusters for which the h1 is present in the
-- >                 CLUSTER key. Parameter can be any range expression.
-- > has(KEY;val)  - returns all clusters with SOMEKEY matching value.
-- > count(EXPR)   - returns the number of results returned by EXPR.
-- > allclusters() - returns the names of all clusters
-- > q(x://blah)   - quote a constant value, the parameter will be returned as
-- >                 is and not evaluated as a range expression. Useful for
-- >                 storing metadata in clusters.
--
-- All of the above can be combined to form highly expressive queries.
--
-- > -- all down redis nodes in the east datacenter.
-- > %{has(DC;east) & has(TYPE;redis)}:DOWN
-- >
-- > -- all clusters with types matching the clusters of host1.
-- > has(TYPE;%{clusters(host1)}:TYPE)
-- >
-- > -- OWNER and DOC values for all clusters on all hosts matching foo.
-- > %{clusters(/foo/)}:{DOC,OWNER}
--
-- = Differences From libcrange
--
-- A number of <https://github.com/square/libcrange/ libcrange> features have
-- been deliberately omitted from hrange, either because they are archaic
-- features of the language, or they are mis-aligned with the goals of this
-- library.
--
-- * @^@ "admin" operator is not supported. Not a useful concept anymore.
-- * @#@ "hash" operator is not supported. Normal function calls are sufficient.
-- * Uses POSIX regular expressions rather than PCRE. Regexes should not be
--   used often anyway: prefer explicit metadata.
-- * Non-deterministic functions, in particular functions that make network
--   calls. This library aims to provide fast query performance, which is much
--   harder when dealing with non-determinism. Clients who wish to emulate
--   this behaviour should either calculate function results upfront and
--   import them into the state, or post-process results.
module Hrange (
    -- * Interacting
      compress
    , expand
    , expand'
    , expandDebug
    -- * Loading
    , analyze
    , analyze'
    , loadStateFromDirectory
    -- * Building States
    , addCluster
    , emptyState
    , makeResult
    -- * Types
    , ClusterEntry
    , ClusterName
    , ParseError
    , Query
    , Result
    , State
    , RangeLog
    ) where

import           Control.Arrow        (first)
import           Control.DeepSeq      (deepseq, ($!!))
import           Control.Lens         ((&), (.~), (^.))
import qualified Data.HashMap.Strict  as M
import qualified Data.HashSet         as S
import qualified Data.Text            as T
import           Data.List            (sort)
import           System.FilePath      (takeBaseName)
import           System.FilePath.Find (always, extension, find, (==?))

import           Hrange.Evaluator (eval, runEval)
import           Hrange.Parser    (ParseError, parseRange)
import           Hrange.Types
import           Hrange.Yaml      (decodeFileWithPath)

type Error = ParseError

-- |Expand a range query against a state into a set of results. Parsing of the
-- query may fail, but evaluation is guaranteed to return results if there are
-- no cycles in the state. (Future versions should protect against cycles, it
-- just hasn't been done yet.) No checking is done for pathological inputs
-- (e.g. @n1..10000000000@).
--
-- >>> expand emptyState "n1..2"
-- Set.fromList ["n1", "n2"]
expand :: State -> Query -> Either Error Result
expand state query = do
  expression <- parseRange Nothing query

  return . fst $ runEval state (eval expression)

-- | Strict version of 'expand'.
expand' :: State -> Query -> Either Error Result
expand' state query =
  case expand state query of
    Left x  -> Left x
    Right x -> x `deepseq` Right x

-- |Same as 'expand', but contains extra debugging information about cache
-- usage.
expandDebug :: State -> Query -> Either Error (Result, [RangeLog])
expandDebug state query = do
  expression <- parseRange Nothing query

  return $ runEval state (eval expression)

-- |Normalizes a range result back into a query. This may not be a minimal
-- compression, but should be shorter in most cases that contain results with
-- repeated elements.
--
-- The current implementation is naive and doesn't actually compress ranges.
--
-- >>> compress (expand emptyState "n1,n2")
-- "n1..2"
compress :: Result -> Query
compress = T.intercalate (T.pack ",") . sort . S.toList

-- |Load a directory of @.yaml@ files into a State. Each file represents a single
-- cluster. Any path that cannot be parsed is returned in the second element of
-- the tuple with an error. Strict. Does not recurse into subdirectories.
loadStateFromDirectory :: FilePath -> IO (State, [(FilePath, String)])
loadStateFromDirectory dir = do
  yamls      <- find always (extension ==? ".yaml") dir
  clusters'  <- mapM decodeFileWithPath yamls
  let clustersWithPaths = zip yamls clusters'
  let validClusters   = [(path, a) | (path, Right a) <- clustersWithPaths]
  let invalidClusters = [(path, a) | (path, Left a) <- clustersWithPaths]

  let clusters'' = M.fromList $
                    map (first (T.pack . takeBaseName)) validClusters

  let state = emptyState & clusters .~ clusters''

  return (state, invalidClusters)

-- |Calculate caches for a state, improving subsequent query performance
-- particularly for reverse lookups such as @clusters@. This is only useful if
-- multiple queries will be run. For running a single query immediately against
-- a state, calling @analyze@ first will result in slower overall time taken.
analyze :: State -> State
analyze state = state & clusterCache .~ newCache
  where
    newCache = Just $!! M.map analyzeCluster (state ^. clusters)

    analyzeCluster :: Cluster -> M.HashMap Identifier Result
    analyzeCluster = M.map runEvalAll

    runEvalAll :: [Expression] -> Result
    runEvalAll = foldl S.union S.empty . map (fst . runEval state . eval)

-- |Strict version of 'analyze'.
analyze' :: State -> State
analyze' state = let state' = analyze state in
  state' `deepseq` state'

-- |A key/results pair for purposes of easily programatically specifying a
-- cluster. This is only used as an input type for certain builder functions -
-- internally a different representation is used.
type ClusterEntry = (ClusterKey, [Identifier])

-- |Builder method for programatically creating a cluster and adding it to a
-- state. This returned state will not be 'analyze'd.
addCluster :: ClusterName -> [ClusterEntry] -> State -> State
addCluster = undefined
--addCluster name cluster = clusters %~ M.insert name cluster
