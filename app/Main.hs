{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

state =
  addCluster "again" (Map.singleton "CLUSTER" (Set.fromList [Const "c"])) $
  addCluster "hello" (Map.singleton "CLUSTER" (Set.fromList [Const "a", GroupLookup (Const "again") (Const "CLUSTER")])) $
  emptyState

main :: IO ()
main = print $ eval state (Subtract (GroupLookup (Const "hello") (Const "CLUSTER")) (Const "a"))
