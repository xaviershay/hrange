{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Parser
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

state =
  addCluster "again" (mkKey "CLUSTER" [mkConst "c"]) $
  addCluster "hello" (mkKey "CLUSTER" [mkConst "a", ClusterLookup (mkConst "again") (mkConst "CLUSTER")]) $
  addCluster "blah"  (mkKey "ALL"     [mkConst "x"]) $
  emptyState

main :: IO ()
--main = print $ runEval $ eval state (Difference (GroupLookup (Const "hello") (Const "CLUSTER")) (Const "a"))
main = do
  print $ parseRange Nothing "/a/"
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
