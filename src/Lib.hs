{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

-- Reducing duplication doesn't make sense for this suggestion
{-# ANN module "HLint: ignore Reduce duplication" #-}

module Lib
    ( module Lib
    ) where

import Yaml
import Types
import Parser
import Control.Arrow (first)
import           Control.Lens           ((^.), at, non, (%~))
import           Control.Monad
import           "mtl" Control.Monad.Identity
import           "mtl" Control.Monad.Reader
import qualified Data.HashMap.Strict    as M
import qualified Data.HashSet           as S
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Text.Printf            (printf)
import           Control.Monad.Except
import           Data.Either
import qualified Data.Yaml              as Y
import           System.FilePath        (takeBaseName)
import           System.FilePath.Find   (find, (==?), always, extension)
import qualified Text.Regex.TDFA        as R
import Control.DeepSeq (deepseq)

runEval :: State -> Eval a -> a
runEval state e = runIdentity (runReaderT e state)

mapFilterM :: (Monad m) => (Cluster -> m Bool) -> ClusterMap -> m ClusterMap
mapFilterM p clusterMap = do
  matching <- filterM (p . snd) (M.toList clusterMap)
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
  nameSet <- eval namesExpr
  keySet  <- eval keysExpr

  matching <- mapFilterM (hasNamesInKeys nameSet keySet) (state ^. clusters)

  return . S.fromList . M.keys $ matching

  where
    hasNamesInKeys names keys cluster = do
      hasAny <- mapM (hasNameInKeys cluster keys) $ S.toList names
      return $ or hasAny

    hasNameInKeys cluster keys name = do
      hasName <- mapM (hasNameInKey cluster name) $ S.toList keys
      return $ or hasName

    hasNameInKey cluster name key = do
      names <- namesAtKey cluster key
      return $ S.member name names

    namesAtKey cluster key = do
      names <- mapM eval (exprsAtKey cluster key)
      return $ foldr S.union S.empty names

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

clusterLookupKey :: State -> Identifier2 -> Identifier2 -> [Expression]
clusterLookupKey state name "KEYS" =
  map toConst $ M.keys $ state
    ^. clusters
    ^. at name . non M.empty

clusterLookupKey state name key =
  state
    ^. clusters
    ^. at name . non M.empty
    ^. at key . non []

emptyState :: State
emptyState = State { _clusters = M.empty }

addCluster :: T.Text -> Cluster -> State -> State
addCluster name cluster = clusters %~ M.insert name cluster

mkKey :: T.Text -> [Expression] -> Cluster
mkKey = M.singleton

-- Strict
decodeFileWithPath :: FilePath -> IO (Either String (FilePath, Cluster))
decodeFileWithPath fpath = do
    content <- Y.decodeFileEither fpath
    let ret = case content of
                Left _ -> Nothing
                Right x -> Just x

    return $! parseClusters (fpath, ret)
  where
    parseClusters :: (FilePath, Maybe Y.Value) -> Either String (FilePath, Cluster)
    parseClusters (_, Nothing) = Left "Invalid YAML"
    parseClusters (path, Just x) = do
      cluster <- runReader (runExceptT $ parseYAML x) (takeBaseName path) :: Either String Cluster

      return $! cluster `deepseq` (path, cluster)

-- Loads a directory of YAML files into a State. Strict. Does not recurse.
loadStateFromDirectory :: FilePath -> IO State
loadStateFromDirectory dir = do
  yamls     <- find always (extension ==? ".yaml") dir
  clusters'  <- mapM decodeFileWithPath yamls

  let clusters'' = M.fromList $
                    map (first (T.pack . takeBaseName)) $
                    rights clusters' -- TODO: How to fail bad ones?

  return State { _clusters = clusters'' }


-- TODO: Don't expose ParserError?
rangeEval :: State -> String -> Either ParseError Result
rangeEval state query = do
  expression <- parseRange Nothing query

  return $ runEval state (eval expression)
