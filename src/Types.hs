{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

-- TODO: These exports are a shit-show
module Types
  ( Identifier2
  , State
  , Eval
  , Query
  , ClusterName
  , ClusterKey
  , ClusterMap
  , Expression(..)
  , Cluster
  , Result
  , ShowableRegex(..)
  , EvaluatedCluster
  , clusters
  , clusterCache
  , emptyState
  , makeResult
  , mkConst
  , toConst
  , makeShowableRegex
  ) where

import           Control.Lens           hiding (Const)
import           Data.Hashable
import qualified Data.Text              as T
import qualified Data.HashMap.Strict    as M
import qualified Data.HashSet           as S
import           Text.Regex.TDFA        as R
import           GHC.Generics
import           Control.Monad.Reader
import           Control.DeepSeq (NFData, rnf)

type Identifier2 = T.Text

toConst :: T.Text -> Expression
toConst k = Const k

mkConst :: String -> Expression
mkConst x = Const . T.pack $ x

-- Regex doesn't implement Show, Eq, etc which is pretty annoying
data ShowableRegex = ShowableRegex String R.Regex

instance Hashable ShowableRegex where
  hashWithSalt salt (ShowableRegex x _) = hashWithSalt salt x

instance Show ShowableRegex where
  show (ShowableRegex x _) = "/" ++ x ++ "/"

instance Eq ShowableRegex where
  (ShowableRegex x _) == (ShowableRegex y _) = x == y

makeShowableRegex :: Monad m => String -> m Expression
makeShowableRegex x = do
  rx <- makeRegexM x

  return . Regexp $ ShowableRegex x (rx :: R.Regex)

instance NFData ShowableRegex where
  rnf (ShowableRegex s r) = r `seq` rnf s

data Expression =
  Intersection Expression Expression |
  Difference Expression Expression |
  Union Expression Expression |
  ClusterLookup Expression Expression |
  Regexp ShowableRegex |
  FunctionHas Expression Expression |
  FunctionMem Expression Expression |
  FunctionClusters Expression |
  FunctionAllClusters |
  Product [Expression] |
  NumericRange Identifier2 Int Integer Integer |
  Const Identifier2

  deriving (Eq, Show, Generic)

instance Hashable Expression
instance NFData Expression

-- Cluster expressions should be unique (i.e. a set), but that doesn't really
-- buy us anything implementation wise. It's easier (and strictly more accurate
-- to the source data) to store as a list.
type Cluster = M.HashMap Identifier2 [Expression]
type ClusterCache = M.HashMap Identifier2 EvaluatedCluster
type EvaluatedCluster = M.HashMap Identifier2 (S.HashSet Identifier2)

-- TODO: newtype this and provide union/intersect implementations to abstract
-- away Set type. Need benchmarks to work with first.
type Result = S.HashSet Identifier2

-- Builder method for making result sets from a list. Useful for testing and
-- examples.
makeResult :: [Identifier2] -> Result
makeResult = S.fromList

type Query = String -- TODO: Make Text
type ClusterName = Identifier2
type ClusterKey = Identifier2
type ClusterMap = M.HashMap Identifier2 Cluster -- TODO: Is PostEval right here?
-- |A state to run queries against. Usually constructed with
-- 'loadStateFromDirectory'.
data State = State {
  _clusters :: ClusterMap,
  _clusterCache :: Maybe ClusterCache
} deriving (Show, Generic)

makeLenses ''State
instance NFData State

-- |A minimal empty 'State'.
emptyState :: State
emptyState = State { _clusters = M.empty, _clusterCache = Nothing }

type Eval a = ReaderT State Identity a
