{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-} -- TODO: Understand what this doe
{-# LANGUAGE DeriveGeneric #-}

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

import GHC.Generics

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

  deriving (Eq, Show, Generic)

instance Hashable Expression

type Cluster = M.HashMap Identifier (S.HashSet Expression)
-- TODO: newtype this and provide union/intersect implementations to abstract
-- away Set type. Need benchmarks to work with first.
type Result = S.HashSet Identifier

data State = State { _clusters :: M.HashMap Identifier Cluster } deriving (Show)
makeLenses ''State

type Eval a = ReaderT State Identity a

runEval :: State -> Eval a -> a
runEval state e = runIdentity (runReaderT e state)

eval :: Expression -> Eval Result
eval (Const id)         = return $ S.singleton id
eval (Union a b)        = S.union        <$> eval a <*> eval b
eval (Intersection a b) = S.intersection <$> eval a <*> eval b
eval (Difference a b)   = S.difference   <$> eval a <*> eval b
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
eval _ = return S.empty

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
