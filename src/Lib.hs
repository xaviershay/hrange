{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-} -- TODO: Understand what this doe

module Lib
    ( module Lib
    ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens hiding (Const)

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import           "mtl" Control.Monad.Identity
import           "mtl" Control.Monad.Reader
import Data.Foldable

type Identifier = T.Text
data Expression =
  Intersection Expression Expression |
  Difference Expression Expression |
  Union Expression Expression |
  ClusterLookup Expression Expression |
  Regexp Identifier | -- TODO: Native regex type
  Function Identifier [Expression] |
  Product [Expression] |
  NumericRange Identifier Identifier Identifier |
  Const Identifier

  deriving (Eq, Ord, Show)

type Cluster = Map.Map Identifier (Set.Set Expression)
-- TODO: newtype this and provide union/intersect implementations to abstract
-- away Set type. Need benchmarks to work with first.
type Result = Set.Set Identifier

data State = State { _clusters :: Map.Map Identifier Cluster }
makeLenses ''State

type Eval a = ReaderT State Identity a

runEval :: State -> Eval a -> a
runEval state e = runIdentity (runReaderT e state)

-- http://stackoverflow.com/questions/13730439/mapm-for-data-set-in-haskell
mapMSet f s = Set.fromList <$> mapM f (Set.toList s)

eval :: Expression -> Eval Result
eval (Const id)         = return $ Set.singleton id
eval (Union a b)        = Set.union        <$> eval a <*> eval b
eval (Intersection a b) = Set.intersection <$> eval a <*> eval b
eval (Difference a b)   = Set.difference   <$> eval a <*> eval b
eval (ClusterLookup names keys) = do
  state   <- ask
  nameSet <- eval names
  keySet  <- eval keys

  results <- mapMSet eval $
    foldMap (\name ->
      foldMap (\key ->
        state
          ^. clusters
          ^. at name . non Map.empty
          ^. at key . non Set.empty
          ) keySet) nameSet

  -- TODO: folding set union maybe not particularly efficient here, N+M on each
  -- fold?
  return $ foldr Set.union Set.empty results

emptyState = State { _clusters = Map.empty }

addCluster :: Identifier -> Cluster -> State -> State
addCluster name cluster = clusters %~ Map.insert name cluster
