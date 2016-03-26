{-# LANGUAGE OverloadedStrings #-}

module Hrange.Evaluator (
  runEval,
  eval
) where

import           Hrange.Types

import           Control.Lens           (at, non, (^.))
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Reader
import qualified Data.HashMap.Strict    as M
import qualified Data.HashSet           as S
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Text.Printf            (printf)
import qualified Text.Regex.TDFA        as R

-- Reducing duplication doesn't make sense for this suggestion
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

runEval :: State -> Eval a -> a
runEval state e = runIdentity (runReaderT e state)

eval :: Expression -> Eval Result
eval (Const name)         = return $! S.singleton name

eval (Union a b)        = S.union        <$> eval a <*> eval b

eval (Intersection (Regexp _) (Regexp _)) = return S.empty
eval (Intersection (Regexp lhs) rhs) = eval (Intersection rhs (Regexp lhs))
eval (Intersection a (Regexp (ShowableRegex _ rx))) = do
  lhs <- eval a
  return $ S.filter (R.matchTest rx . T.unpack) lhs

eval (Intersection a b) = S.intersection <$> eval a <*> eval b

eval (Difference a (Regexp (ShowableRegex _ rx))) = do
  lhs <- eval a
  return $ S.filter (not . R.matchTest rx . T.unpack) lhs

eval (Difference a b)   = S.difference   <$> eval a <*> eval b

eval FunctionAllClusters = do
  state <- ask
  return . S.fromList . M.keys $ state ^. clusters

eval (FunctionClusters names) = eval $ FunctionHas (mkConst "CLUSTER") names

eval (FunctionMem clustersExpr namesExpr) = do
  clustersSet <- eval clustersExpr
  nameSet     <- eval namesExpr

  clusterKeys     <- mapM (\c -> eval (ClusterLookup (Const c) (Const "KEYS"))) (S.toList clustersSet)
  keysWithResults <- zipWithM keyResults (S.toList clustersSet) (map S.toList clusterKeys)

  let keysWithResults' = foldl M.union M.empty keysWithResults :: M.HashMap ClusterKey Result

  let matching = M.filter (\rs -> any (`S.member` rs) (S.toList nameSet)) keysWithResults'

  return . S.fromList . M.keys $ matching

  where
    keyResults :: ClusterName -> [ClusterKey] -> Eval (M.HashMap ClusterKey Result)
    keyResults name ks = do
      results <- mapM (eval . ClusterLookup (Const name) . Const) ks

      return . M.fromList $ zip ks results

eval (FunctionHas keysExpr namesExpr) = do
  state   <- ask
  keySet  <- eval keysExpr
  nameSet <- eval namesExpr

  matching <- filterMapM (hasNamesInKeys nameSet keySet) (state ^. clusters)

  return . S.fromList . M.keys $ matching

  where
    filterMapM :: (Monad m) => ((Identifier, Cluster) -> m Bool) -> ClusterMap -> m ClusterMap
    filterMapM p clusterMap = do
      matching <- filterM p (M.toList clusterMap)
      return $ M.fromList matching

eval (Product []) = return S.empty
eval (Product xs) = do
  results <- mapM eval xs

  let asList   = map S.toList results :: [[Identifier]]
  let combined = map T.concat $ sequence asList :: [T.Text]

  return . S.fromList $ combined

eval (ClusterLookup namesExpr keysExpr) = do
  state   <- ask
  nameSet <- eval namesExpr
  keySet  <- eval keysExpr

  results <- mapM eval $
    foldMap (\name ->
      foldMap (clusterLookupKey state name) keySet) nameSet

  return $ foldr S.union S.empty results

eval (NumericRange prefix width low high) = do
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

hasNamesInKeys :: S.HashSet ClusterName -> S.HashSet ClusterKey -> (ClusterName, Cluster) -> Eval Bool
hasNamesInKeys names keys (_, cluster) = do
  hasAny <- mapM (hasNameInKeys cluster keys) $ S.toList names
  return $ or hasAny

hasNameInKeys :: Cluster -> S.HashSet ClusterKey -> ClusterName -> Eval Bool
hasNameInKeys cluster keys name = do
  hasName <- mapM (hasNameInKey cluster name) $ S.toList keys
  return $ or hasName

hasNameInKey :: Cluster -> ClusterName -> ClusterKey -> Eval Bool
hasNameInKey c name key = do
  names <- namesAtKey c key "bogus"
  return $ S.member name names

namesAtKey :: Cluster -> ClusterKey -> ClusterName -> Eval Result
namesAtKey cluster key name = do
  state <- ask

  -- TODO This doesn't work, still gets cache misses
  let cache = state ^. clusterCache . non M.empty
  let maybeCache = cache ^. at name :: Maybe EvaluatedCluster
  let cacheMiss = do
                    vs <- mapM eval (exprsAtKey cluster key)
                    return $ foldr S.union S.empty vs

  case maybeCache of
    Just c  -> return $ c ^. at key . non S.empty
    Nothing -> cacheMiss

  --maybe cacheMiss cacheHit clusterCache

exprsAtKey :: Cluster -> Identifier -> [Expression]
exprsAtKey c key = c ^. at key . non []

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
