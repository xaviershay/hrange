{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Lib
    ( module Lib
    ) where

import Yaml
import Types
import Parser
import           Control.Applicative    ((<$>), (<*>))
import           Control.Lens           hiding (Const)
import           Control.Monad
import           "mtl" Control.Monad.Identity
import           "mtl" Control.Monad.Reader
import           Data.Foldable  hiding (find)
import qualified Data.HashMap.Strict    as M
import qualified Data.HashSet           as S
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           GHC.Generics
import           Text.Printf            (printf)
import           Control.Monad.Except
import           Data.Either
import qualified Data.Yaml              as Y
import           System.FilePath        (takeBaseName, takeDirectory)
import           System.FilePath.Find
import           Text.Regex.TDFA        as R

runEval :: State -> Eval a -> a
runEval state e = runIdentity (runReaderT e state)

mapFilterM :: (Monad m) => (Cluster -> m Bool) -> ClusterMap -> m ClusterMap
mapFilterM p clusters = do
  matching <- filterM (p . snd) (M.toList clusters)
  return $ M.fromList matching

eval :: Expression -> Eval Result
eval (Const id)         = return $ S.singleton (toResult id)
  where
    toResult (Identifier x) = x

eval (Union a b)        = S.union        <$> eval a <*> eval b

-- TODO: Fail parse if two regexps
eval (Intersection (Regexp lhs) (Regexp rhs)) = return S.empty
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
eval (FunctionHas keys names) = do
  state <- ask
  nameSet <- eval names
  keySet  <- eval keys

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

eval (Product xs) = do
  results <- mapM eval xs

  let asList   = map S.toList results :: [[Identifier2]]
  let combined = map T.concat $ sequence (map (map id) asList) :: [T.Text]

  return . S.fromList $ combined


eval (ClusterLookup names keys) = do
  state   <- ask
  nameSet <- eval names
  keySet  <- eval keys

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

emptyState = State { _clusters = M.empty }

addCluster :: T.Text -> Cluster -> State -> State
addCluster name cluster = clusters %~ M.insert name cluster

mkKey :: T.Text -> [Expression] -> Cluster
mkKey name = M.singleton name

fromMap x = State { _clusters = x }

decodeFileWithPath path = do
    content <- Y.decodeFile path
    return $ True `seq` (path, content)

loadStateFromDirectory dir = do
  yamls    <- find always (extension ==? ".yaml") dir
  raw      <- mapM decodeFileWithPath yamls

  let clusters = map parseClusters raw
  let clusters' = M.fromList $
                    map (\(k, v) -> ((T.pack . takeBaseName $ k), v)) $
                    rights clusters -- TODO: How to fail bad ones?

  return $ State { _clusters = clusters' }

parseClusters (path, Nothing) = fail "Invalid YAML"
parseClusters (path, Just x) = do
  cluster <- runReader (runExceptT $ parseYAML x) (takeBaseName path)

  return (path, cluster)

rangeEval state query = do
  expression <- parseRange Nothing query

  return $ runEval state (eval expression)
