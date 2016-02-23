{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

state = addCluster 
  (addCluster emptyState "hello" $ Map.singleton "CLUSTER" (Set.fromList [Const "a", GroupLookup (Const "again") (Const "CLUSTER")]))
  "again" $ Map.singleton "CLUSTER" (Set.fromList [Const "c"])

main :: IO ()
main = putStrLn . show $ eval state (GroupLookup (Const "hello") (Const "CLUSTER"))
