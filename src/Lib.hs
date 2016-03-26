{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Lib
    ( analyzeCluster
    ) where

import Hrange.Evaluator
import Yaml
import Types

import           Control.DeepSeq        (deepseq)
import           Control.Lens           (at, non, (^.))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import qualified Data.HashMap.Strict    as M
import qualified Data.HashSet           as S
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import qualified Data.Yaml              as Y
import           System.FilePath        (takeBaseName)
import           Text.Printf            (printf)
import qualified Text.Regex.TDFA        as R

analyzeCluster :: State -> Cluster -> M.HashMap Identifier Result
analyzeCluster state = M.map (runEvalAll state)

runEvalAll :: State -> [Expression] -> Result
runEvalAll state = foldl S.union S.empty . map (runEval state . eval)
