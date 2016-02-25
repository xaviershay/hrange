{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Parser
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

state =
  addCluster "again" (Map.singleton "CLUSTER" (Set.fromList [Const "c"])) $
  addCluster "hello" (Map.singleton "CLUSTER" (Set.fromList [Const "a", ClusterLookup (Const "again") (Const "CLUSTER")])) $
  addCluster "blah" (Map.singleton "ALL" (Set.fromList [Const "x"])) $
  emptyState

main :: IO ()
--main = print $ runEval $ eval state (Difference (GroupLookup (Const "hello") (Const "CLUSTER")) (Const "a"))
main = do
  print $ parseRange "n1..2"
  --print $ parseRange "%hello & there"
  --print $ parseRange "%hello"
  --print $ parseRange "%hello:KEYS"
  --print $ parseRange "a"
  --print $ parseRange "a,b,c"
  --print $ parseRange "a{b,c}"
  --print $ parseRange "a{b,c}d"
  --print $ parseRange "{a,b}{c,d}"
  --print $ parseRange "%{a,b}:{c,d}"
  --print $ parseRange "%hello-there"
  --print $ parseRange "hello-there"
  --print $ parseRange "hello - there"
  --print $ parseRange "a & /a/"
  --print $ parseRange "{%abc,b}"
