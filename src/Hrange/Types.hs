{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

-- TODO: These exports are a shit-show
module Hrange.Types
  ( Identifier
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
  , RangeLog(..)
  , clusters
  , clusterCache
  , emptyState
  , makeResult
  , mkConst
  , toConst
  , makeShowableRegex
  , recordStat
  ) where

import           Control.Lens           hiding (Const)
import           Data.Hashable
import qualified Data.Text              as T
import qualified Data.HashMap.Strict    as M
import qualified Data.HashSet           as S
import qualified Data.Sequence          as Seq
import           Text.Regex.TDFA        as R
import           GHC.Generics
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.DeepSeq (NFData, rnf)

-- |Any arbitrary range string identifier.
type Identifier = T.Text

toConst :: T.Text -> Expression
toConst = Const

mkConst :: String -> Expression
mkConst = Const . T.pack

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
  NumericRange Identifier Int Integer Integer |
  Const Identifier

  deriving (Eq, Show, Generic)

instance Hashable Expression
instance NFData Expression

-- Cluster expressions should be unique (i.e. a set), but that doesn't really
-- buy us anything implementation wise. It's easier (and strictly more accurate
-- to the source data) to store as a list.
type Cluster = M.HashMap Identifier [Expression]
type ClusterCache = M.HashMap Identifier EvaluatedCluster
type EvaluatedCluster = M.HashMap Identifier (S.HashSet Identifier)

-- TODO: newtype this and provide union/intersect implementations to abstract
-- away Set type. Need benchmarks to work with first.
type Result = S.HashSet Identifier

-- Builder method for making result sets from a list. Useful for testing and
-- examples.
makeResult :: [Identifier] -> Result
makeResult = S.fromList

type Query = String -- TODO: Make Text
type ClusterName = Identifier
type ClusterKey = Identifier
type ClusterMap = M.HashMap Identifier Cluster
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

type Eval a = WriterT (Seq.Seq RangeLog) (ReaderT State Identity) a

-- Allows results to be folded together without needing to unwrap them first.
instance Monoid a => Monoid (Eval a) where
  mempty      = return mempty
  mappend a b = mappend <$> a <*> b

data RangeLog = CacheHit Identifier | CacheMiss Identifier deriving (Show)

recordStat :: RangeLog -> Eval ()
recordStat = tell . Seq.singleton

