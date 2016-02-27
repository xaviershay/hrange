{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Parser
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

state =
  addCluster "again" (M.singleton "CLUSTER" (S.fromList [Const "c"])) $
  addCluster "hello" (M.singleton "CLUSTER" (S.fromList [Const "a", ClusterLookup (Const "again") (Const "CLUSTER")])) $
  addCluster "blah" (M.singleton "ALL" (S.fromList [Const "x"])) $
  emptyState

main :: IO ()
--main = print $ runEval $ eval state (Difference (GroupLookup (Const "hello") (Const "CLUSTER")) (Const "a"))
main = do
  print $ parseRange "/a/"
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
