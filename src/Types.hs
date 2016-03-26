{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Types
  ( module Types
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

-- TODO: Don't export Identifier, provide constructors.
newtype Identifier a = Identifier T.Text deriving (Show, Eq, Generic)
instance Hashable (Identifier a)
instance NFData (Identifier a)

type Identifier2 = T.Text

data PreEval
data PostEval

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
  NumericRange (Identifier PreEval) Int Integer Integer |
  Const (Identifier2)

  deriving (Eq, Show, Generic)

instance Hashable Expression
instance NFData Expression

-- Cluster expressions should be unique (i.e. a set), but that doesn't really
-- buy us anything implementation wise. It's easier (and strictly more accurate
-- to the source data) to store as a list.
type Cluster = M.HashMap Identifier2 [Expression]
type ReverseClusterMap = M.HashMap (Identifier2, Identifier2) (S.HashSet Identifier2)
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

type Eval a = ReaderT State Identity a
