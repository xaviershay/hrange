{-# LANGUAGE OverloadedStrings #-}

module Logging
  ( logInfoReq
  , logInfo
  , spawnLogThread
  , fromText
  , build
  , fixed
  , Only(..)
  ) where

import Prelude hiding (print)

import           Control.Concurrent             (ThreadId, forkIO)
import           Control.Concurrent.BoundedChan
import           Control.Monad                  (forever)
import           Data.Text.Buildable            (Buildable)
import           Data.Text.Format
import qualified Data.Text.Lazy                 as TL
import           Data.Text.Lazy.Builder         (fromText)
import           Data.Time.Clock                (getCurrentTime)
import           Network.Wai
import           System.IO.Unsafe               (unsafePerformIO)

-- This is a hack to allow global logging in vanilla IO monad. Should wrap this
-- up in a custom app monad or something.
logChannel :: BoundedChan a
{-# NOINLINE logChannel #-}
logChannel = unsafePerformIO (newBoundedChan logBufferSize)

-- Completely arbitrary bound for log buffer.
logBufferSize :: Int
logBufferSize = 100

-- A separate logging thread is used to prevent interleaving of distinct log
-- messages, while also preventing synchronization on the main thread. It's
-- mostly just cute, should actually benchmark against the non-background
-- thread approach.
spawnLogThread :: IO ThreadId
spawnLogThread = forkIO . forever $
  readChan logChannel >>= putStrLn . TL.unpack

logInfoReq :: Buildable a => Request -> a -> IO ()
logInfoReq req msg = do
    let remote = show $ remoteHost req

    logInfo $ build "{} {}" (remote, msg)

logInfo :: Buildable a => a -> IO ()
logInfo msg = do
    let level = "INFO" :: String

    currentTime <- getCurrentTime
    _ <- tryWriteChan logChannel $
           format "{} [{}] {}" (right 5 ' ' level, show currentTime, msg)

    return ()
