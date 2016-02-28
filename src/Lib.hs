{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-} -- TODO: Understand what this doe
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( module Lib
    ) where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Control.Lens hiding (Const)

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import           "mtl" Control.Monad.Identity
import           "mtl" Control.Monad.Reader
import Data.Foldable
import Data.Hashable
import Data.Monoid ((<>))

import Text.Printf (printf)
import Text.Regex.TDFA as R

import GHC.Generics

type Identifier = T.Text

-- Regex doesn't implement Show, Eq, etc which is pretty annoying
data ShowableRegex = ShowableRegex String R.Regex

instance Hashable ShowableRegex where
  hashWithSalt salt (ShowableRegex x _) = hashWithSalt salt x

instance Show ShowableRegex where
  show (ShowableRegex x _) = "/" ++ x ++ "/"

instance Eq ShowableRegex where
  (ShowableRegex x _) == (ShowableRegex y _) = x == y

makeShowableRegex x = do
  rx <- makeRegexM x

  return . Regexp $ ShowableRegex x (rx :: R.Regex)

data Expression =
  Intersection Expression Expression |
  Difference Expression Expression |
  Union Expression Expression |
  ClusterLookup Expression Expression |
  Regexp ShowableRegex |
  FunctionHas Expression Expression |
  FunctionClusters Expression |
  FunctionAllClusters |
  Product [Expression] |
  NumericRange Identifier Int Integer Integer |
  Const Identifier

  deriving (Eq, Show, Generic)


instance Hashable Expression

type Cluster = M.HashMap Identifier (S.HashSet Expression)
-- TODO: newtype this and provide union/intersect implementations to abstract
-- away Set type. Need benchmarks to work with first.
type Result = S.HashSet Identifier
type ClusterMap = M.HashMap Identifier Cluster
data State = State { _clusters :: ClusterMap } deriving (Show)
makeLenses ''State

type Eval a = ReaderT State Identity a

runEval :: State -> Eval a -> a
runEval state e = runIdentity (runReaderT e state)

mapFilterM :: (Monad m) => (Cluster -> m Bool) -> ClusterMap -> m ClusterMap
mapFilterM p clusters = do
  matching <- filterM (p . snd) (M.toList clusters)
  return $ M.fromList matching

eval :: Expression -> Eval Result
eval (Const id)         = return $ S.singleton id
eval (Union a b)        = S.union        <$> eval a <*> eval b

-- TODO: Fail parse if two regexps
eval (Intersection (Regexp lhs) (Regexp rhs)) = error "Invalid, should be prevented by parse"
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
eval (FunctionClusters names) = eval $ FunctionHas (Const "CLUSTER") names

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
      names <- mapM eval . S.toList $ exprsAtKey cluster key
      return $ foldr S.union S.empty names

    exprsAtKey :: Cluster -> Identifier -> S.HashSet Expression
    exprsAtKey cluster key = cluster ^. at key . non S.empty

eval (Product xs) = do
  results <- mapM eval xs

  let asList   = map S.toList results :: [[Identifier]]
  let combined = map T.concat $ sequence asList :: [Identifier]

  return . S.fromList $ combined

eval (ClusterLookup names keys) = do
  state   <- ask
  nameSet <- eval names
  keySet  <- eval keys

  results <- mapM eval . S.toList $
    foldMap (\name ->
      foldMap (clusterLookupKey state name) keySet) nameSet

  -- TODO: folding set union maybe not particularly efficient here?
  return $ foldr S.union S.empty results

eval (NumericRange prefix width low high) = do
  let nums = map (T.pack . printf ("%0" ++ show width ++ "i")) [low..high] :: [Identifier]

  return . S.fromList $ map (prefix <>) nums

clusterLookupKey :: State -> Identifier -> Identifier -> S.HashSet Expression
clusterLookupKey state name "KEYS" =
  S.fromList $ map Const $ M.keys $ state
    ^. clusters
    ^. at name . non M.empty

clusterLookupKey state name key =
  state
    ^. clusters
    ^. at name . non M.empty
    ^. at key . non S.empty

emptyState = State { _clusters = M.empty }

addCluster :: Identifier -> Cluster -> State -> State
addCluster name cluster = clusters %~ M.insert name cluster

fromMap x = State { _clusters = x }
