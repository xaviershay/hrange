{-# LANGUAGE OverloadedStrings #-}

module Hrange.Text
  ( T.concat
  , leftpad
  , match
  ) where

import           Hrange.Types

import qualified Data.Text           as T
import           Data.Text.Buildable (Buildable)
import           Data.Text.Format
import           Data.Text.Lazy      (toStrict)
import           Text.Regex.TDFA     (matchTest)

leftpad :: (Buildable a) => Int -> a -> T.Text
leftpad width x = toStrict $ format "{}" [left width '0' x]

-- TODO: How to not convert back to String here?
match :: ShowableRegex -> T.Text -> Bool
match (ShowableRegex _ rx) = matchTest rx . T.unpack
