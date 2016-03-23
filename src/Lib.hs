{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Lib
    ( module Lib
    ) where


import Debug.Trace
import Yaml
import Types
import Parser
import Control.Arrow (first)
import           Control.Lens           ((^.), at, non, (%~), (.~), (&), set)
import           Control.Monad
import           "mtl" Control.Monad.Identity
import           "mtl" Control.Monad.Reader
import qualified Data.HashMap.Strict    as M
import qualified Data.HashSet           as S
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import Data.Maybe (catMaybes, fromJust)
import           Text.Printf            (printf)
import           Control.Monad.Except
import           Data.Either
import qualified Data.Yaml              as Y
import           System.FilePath        (takeBaseName)
import           System.FilePath.Find   (find, (==?), always, extension)
import qualified Text.Regex.TDFA        as R
import Control.DeepSeq (deepseq, ($!!))

-- Reducing duplication doesn't make sense for this suggestion
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

runEval :: State -> Eval a -> a
runEval state e = runIdentity (runReaderT e state)

mapFilterM :: (Monad m) => ((Identifier2, Cluster) -> m Bool) -> ClusterMap -> m ClusterMap
mapFilterM p clusterMap = do
  matching <- filterM p (M.toList clusterMap)
  return $ M.fromList matching

eval :: Expression -> Eval Result
eval (Const name)         = return $! S.singleton (toResult name)
  where
    toResult (Identifier x) = x

eval (Union a b)        = S.union        <$> eval a <*> eval b

-- TODO: Fail parse if two regexps
eval (Intersection (Regexp _) (Regexp _)) = return S.empty
eval (Intersection (Regexp lhs) rhs) = eval (Intersection rhs (Regexp lhs))
eval (Intersection a (Regexp (ShowableRegex _ rx))) = do
  lhs <- eval a
  return $ S.filter (R.matchTest rx . T.unpack) lhs

eval (Intersection a b) = S.intersection <$> eval a <*> eval b

-- TODO: Fail parse if diff on LHS
eval (Difference a (Regexp (ShowableRegex _ rx))) = do
  lhs <- eval a
  return $ S.filter (not . R.matchTest rx . T.unpack) lhs

eval (Difference a b)   = S.difference   <$> eval a <*> eval b

-- TODO: Type checking for number of args
eval FunctionAllClusters = do
  state <- ask
  return . S.fromList . M.keys $ state ^. clusters

-- TODO: Type checking for number of args
eval (FunctionClusters names) = eval $ FunctionHas (mkConst "CLUSTER") names

-- TODO: Type checking for number of args
eval (FunctionHas keysExpr namesExpr) = do
  state <- ask
  keySet  <- eval keysExpr
  nameSet <- eval namesExpr

  matching <- mapFilterM (hasNamesInKeys nameSet keySet) (state ^. clusters)

  return . S.fromList . M.keys $ matching

  where
    hasNamesInKeys :: S.HashSet Identifier2 -> S.HashSet Identifier2 -> (Identifier2, Cluster) -> Eval Bool
    hasNamesInKeys names keys (clusterName, cluster) = do
      hasAny <- mapM (hasNameInKeys cluster keys) $ S.toList names
      return $ or hasAny

      where
        hasNameInKeys :: Cluster -> S.HashSet Identifier2 -> Identifier2 -> Eval Bool
        hasNameInKeys cluster keys name = do
          hasName <- mapM (hasNameInKey cluster name) $ S.toList keys
          return $ or hasName

        hasNameInKey :: Cluster -> Identifier2 -> Identifier2 -> Eval Bool
        hasNameInKey cluster name key = do
          names <- namesAtKey cluster key "bogus"
          return $ S.member name names

        namesAtKey :: Cluster -> Identifier2 -> Identifier2 -> Eval Result
        namesAtKey cluster key clusterName = do
          state <- ask

          -- TODO This doesn't work, still gets cache misses
          let cache = state ^. clusterCache . non M.empty
          let clusterCache = cache ^. at clusterName :: Maybe EvaluatedCluster
          let cacheMiss = do
                            names <- mapM eval (exprsAtKey cluster key)
                            return $ foldr S.union S.empty names

          case clusterCache of
            Just c -> return $ c ^. at key . non S.empty
            Nothing -> cacheMiss

          --maybe cacheMiss cacheHit clusterCache

        exprsAtKey :: Cluster -> Identifier2 -> [Expression]
        exprsAtKey cluster key = cluster ^. at key . non []

eval (Product []) = return S.empty
eval (Product xs) = do
  results <- mapM eval xs

  let asList   = map S.toList results :: [[Identifier2]]
  let combined = map T.concat $ sequence asList :: [T.Text]

  return . S.fromList $ combined


eval (ClusterLookup namesExpr keysExpr) = do
  state   <- ask
  nameSet <- eval namesExpr
  keySet  <- eval keysExpr

  results <- mapM eval $
    foldMap (\name ->
      foldMap (clusterLookupKey state name) keySet) nameSet

  -- TODO: folding set union maybe not particularly efficient here?
  return $ foldr S.union S.empty results

eval (NumericRange (Identifier prefix) width low high) = do
  let nums = map (T.pack . printf ("%0" ++ show width ++ "i")) [low..high] :: [T.Text]

  return . S.fromList $ map (prefix <>) nums

-- Some implementations return %{allclusters()} matched against the regex. On a
-- suspicion that this a pattern that should be discouraged, I'm opting here to
-- return empty and prevent the parser from generating regex expression outside
-- of an intersection or difference. Those implementations provide special
-- cased actual behaviour for Regexp without recursing here.
--
-- If this turns out to be a good decision, I'll consider encoding it into the
-- type system.
eval (Regexp _) = return S.empty

clusterLookupKey :: State -> ClusterName -> ClusterKey -> [Expression]
clusterLookupKey state name "KEYS" =
  map toConst $ M.keys $ state
    ^. clusters
    ^. at name . non M.empty

clusterLookupKey state name key =
  state
    ^. clusters
    ^. at name . non M.empty
    ^. at key . non []

-- Strict
decodeFileWithPath :: FilePath -> IO (Either String Cluster)
decodeFileWithPath fpath = do
    content <- Y.decodeFileEither fpath
    let ret = case content of
                Left _ -> Nothing
                Right x -> Just x

    return $! parseClusters (fpath, ret)
  where
    parseClusters :: (FilePath, Maybe Y.Value) -> Either String Cluster
    parseClusters (_, Nothing) = Left "Invalid YAML"
    parseClusters (path, Just x) = do
      cluster <- runReader (runExceptT $ parseYAML x) (takeBaseName path) :: Either String Cluster

      return $! cluster `deepseq` cluster

analyzeCluster :: State -> Cluster -> M.HashMap Identifier2 Result
analyzeCluster state = M.map (runEvalAll state)

runEvalAll :: State -> [Expression] -> Result
runEvalAll state = foldl S.union S.empty . map (runEval state . eval)

-- Flattens a cluster map, returning a tuple (name, key, expr) for every entry.
allEntries :: ClusterMap -> [(Identifier2, Identifier2, Expression)]
allEntries clusterMap = concatMap (\(name, cluster) ->
                          concatMap (\(key, exprs) ->
                            map (\expr -> (name, key, expr)) exprs)
                          (M.toList cluster))
                        (M.toList clusterMap)

buildIndex :: ClusterMap -> ReverseClusterMap
buildIndex clusterMap =
  let entries = allEntries clusterMap in
  let staticEntries = catMaybes $ map evalConst entries in

  M.fromListWith S.union $ map (\(n, k, e) -> ((e, k), S.singleton n)) staticEntries

  where
    evalConst (n, k, Const x) = Just (n, k, toResult x)
    evalConst (n, k, x)       = Nothing -- TODO: Need to return these and index separately

    toResult (Identifier x) = x

-- TODO: Don't expose ParserError?
rangeEval :: State -> String -> Either ParseError Result
rangeEval state query = do
  expression <- parseRange Nothing query

  return $ runEval state (eval expression)
