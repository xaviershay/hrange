module Hrange
    ( State
    , Result
    , ParseError
    , Query
    , expand
    , compress
    , loadStateFromDirectory
    , analyze
    , analyze'
    , emptyState
    ) where

import Lib (runEval, eval, decodeFileWithPath, analyzeCluster)
import Types
import Parser (parseRange, ParseError)
import Control.Lens           ((^.), (.~), (&))
import           System.FilePath        (takeBaseName)
import System.FilePath.Find   (find, (==?), always, extension)
import Control.Arrow (first)
import Control.DeepSeq (($!!), deepseq)
import qualified Data.HashMap.Strict    as M
import qualified Data.Text              as T

type Error = ParseError

expand :: State -> Query -> Either Error Result
expand state query = do
  expression <- parseRange Nothing query

  return $ runEval state (eval expression)

compress :: Result -> CompressedResult
compress = undefined

-- Loads a directory of YAML files into a State. Strict. Does not recurse into subdirectories.
loadStateFromDirectory :: FilePath -> IO (State, [(FilePath, String)])
loadStateFromDirectory dir = do
  yamls      <- find always (extension ==? ".yaml") dir
  clusters'  <- mapM decodeFileWithPath yamls
  let clustersWithPaths = zip yamls clusters'
  let validClusters   = [(path, a) | (path, Right a) <- clustersWithPaths]
  let invalidClusters = [(path, a) | (path, Left a) <- clustersWithPaths]

  let clusters'' = M.fromList $
                    map (first (T.pack . takeBaseName)) validClusters

  let state = State { _clusters = clusters'', _clusterCache = Nothing }

  return (state, invalidClusters)

analyze :: State -> State
analyze state = state & clusterCache .~ newCache
  where
    newCache = Just $!! M.map (analyzeCluster state) (state ^. clusters)

-- Strict version of analyze
analyze' :: State -> State
analyze' state = let state' = analyze state in
  state' `deepseq` state'

emptyState :: State
emptyState = State { _clusters = M.empty, _clusterCache = Nothing }

