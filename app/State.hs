{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module State
  ( StateContainer
  , newStateContainer
  , updateStateContainer
  , mostRecent
  , history
  , toIdentifier
  ) where

import           Hrange
import           Control.Lens           hiding (Const)
import           Data.Cache.LRU           (newLRU, insert, lookup, LRU)
import           Data.Hashable            (hash)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import           GHC.Generics

data StateContainer = StateContainer {
  _mostRecent :: State,
  _history    :: LRU T.Text State
} deriving (Show, Generic)
makeLenses ''StateContainer

newStateContainer = StateContainer {
  _mostRecent = emptyState,
  _history    = newLRU (Just 5)
}

updateStateContainer stamp state container = 
  let StateContainer { _history = history } = container in
  container {
    _mostRecent = state,
    _history    = insert stamp state history
  }

toIdentifier :: State -> T.Text
toIdentifier state = "v1-" <> (T.pack . show . hash) state
