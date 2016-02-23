{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( module Lib
    ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens hiding (Const)

type Identifier = T.Text
data Expression =
  GroupLookup Expression Expression |
  Const Identifier

  deriving (Eq, Ord, Show)

type Cluster = Map.Map Identifier (Set.Set Expression)
type Result = Set.Set Identifier

data State = State { _clusters :: Map.Map Identifier Cluster }
makeLenses ''State

-- TODO: folding set union maybe not particularly efficient here, N+M on each
-- fold?
eval :: State -> Expression -> Result
eval state (Const id) = Set.singleton id
eval state (GroupLookup names keys) =
  foldr Set.union Set.empty $
    Set.map (eval state) $
      state
        ^. clusters
        ^. at (Set.findMin nameSet) . non Map.empty
        ^. at (Set.findMin keySet) . non Set.empty
  where
    nameSet = eval state names
    keySet  = eval state keys

emptyState = State { _clusters = Map.empty }

addCluster :: Identifier -> Cluster -> State -> State
addCluster name cluster = clusters %~ Map.insert name cluster
