{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

state =
  addCluster "again" (Map.singleton "CLUSTER" (Set.fromList [Const "c"])) $
  addCluster "hello" (Map.singleton "CLUSTER" (Set.fromList [Const "a", GroupLookup (Const "again") (Const "CLUSTER")])) $
  addCluster "blah" (Map.singleton "ALL" (Set.fromList [Const "x"])) $
  emptyState

main :: IO ()
--main = print $ runEval $ eval state (Difference (GroupLookup (Const "hello") (Const "CLUSTER")) (Const "a"))
main = print $ runEval state $ eval (GroupLookup (Union (Const "hello") (Const "blah")) (Union (Const "ALL") (Const "CLUSTER")))
