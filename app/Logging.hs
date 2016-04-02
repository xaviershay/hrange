{-# LANGUAGE OverloadedStrings #-}

module Logging
  ( logInfoReq
  , logInfo
  , fromText
  , build
  , fixed
  , Only(..)
  ) where

import Prelude hiding (print)

import           Data.Text.Buildable    (Buildable)
import           Data.Text.Format
import           Data.Text.Lazy.Builder (fromText)
import           Data.Time.Clock        (getCurrentTime)
import           Network.Wai

logInfoReq :: Buildable a => Request -> a -> IO ()
logInfoReq req msg = do
    let remote = show $ remoteHost req

    logInfo $ build "{} {}" (remote, msg)

logInfo :: Buildable a => a -> IO ()
logInfo msg = do
    let level = "INFO" :: String

    currentTime <- getCurrentTime
    print "{} [{}] {}\n" (right 5 ' ' level, show currentTime, msg)
