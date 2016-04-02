{-# LANGUAGE OverloadedStrings #-}

module Hrange.Text
  ( T.concat
  , leftpad
  , match
  , commonSuffix
  , stripChars
  ) where

import           Hrange.Types

import           Data.List           (transpose)
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

commonSuffix :: [String] -> String
commonSuffix xs =
  reverse . map head . takeWhile elemsAreSame . transpose . map reverse $ xs
  where
    l = length xs - 1

    elemsAreSame :: Eq a => [a] -> Bool
    elemsAreSame []     = False
    elemsAreSame (c:cs) = length cs == l && all (== c) cs

-- http://www.rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string#Haskell
stripChars :: String -> String -> String
stripChars = filter . flip notElem
