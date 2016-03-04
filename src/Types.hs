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
import           Control.Monad.Identity
import           Control.Monad.Reader

-- TODO: Don't export Identifier, provide constructors.
newtype Identifier a = Identifier T.Text deriving (Show, Eq, Generic)
instance Hashable (Identifier a)

data PreEval
data PostEval

toConst (Identifier k) = Const (Identifier k)

mkConst x = Const $ Identifier . T.pack $ x


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
  NumericRange (Identifier PreEval) Int Integer Integer |
  Const (Identifier PreEval)

  deriving (Eq, Show, Generic)

instance Hashable Expression

-- Cluster expressions should be unique (i.e. a set), but that doesn't really
-- buy us anything implementation wise. It's easier (and strictly more accurate
-- to the source data) to store as a list.
type Cluster = M.HashMap (Identifier PostEval) [Expression]

-- TODO: newtype this and provide union/intersect implementations to abstract
-- away Set type. Need benchmarks to work with first.
type Result = S.HashSet (Identifier PostEval)
type ClusterMap = M.HashMap (Identifier PostEval) Cluster -- TODO: Is PostEval right here?
data State = State { _clusters :: ClusterMap } deriving (Show)
makeLenses ''State

type Eval a = ReaderT State Identity a