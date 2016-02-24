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

import Control.Monad
import           "mtl" Control.Monad.Identity
import Data.Foldable

type Identifier = T.Text
data Expression =
  Intersection Expression Expression |
  Difference Expression Expression |
  Union Expression Expression |
  GroupLookup Expression Expression |
  Const Identifier

  deriving (Eq, Ord, Show)

type Cluster = Map.Map Identifier (Set.Set Expression)
type Result = Set.Set Identifier

data State = State { _clusters :: Map.Map Identifier Cluster }
makeLenses ''State

type Eval a = Identity a

runEval :: Eval a -> a
runEval = runIdentity

-- http://stackoverflow.com/questions/13730439/mapm-for-data-set-in-haskell
mapMSet f s = Set.fromList <$> mapM f (Set.toList s)

eval :: State -> Expression -> Eval Result
eval state (Const id)         = return $ Set.singleton id
eval state (Union a b)        = liftM2 Set.union        (eval state a) (eval state b)
eval state (Intersection a b) = liftM2 Set.intersection (eval state a) (eval state b)
eval state (Difference a b)   = liftM2 Set.difference   (eval state a) (eval state b)
eval state (GroupLookup names keys) = do
  nameSet <- eval state names
  keySet  <- eval state keys

  results <- mapMSet (eval state) $
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
