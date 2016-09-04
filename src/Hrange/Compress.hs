{-# LANGUAGE OverloadedStrings #-}

module Hrange.Compress (
      compress
    , compressRanges
    ) where

import qualified Algorithms.NaturalSort as NS
import qualified Data.HashMap.Strict  as M
import qualified Data.HashSet         as S
import           Data.List            (sortBy)
import           Data.Monoid          ((<>))
import qualified Data.Text            as T
import           Text.Regex.TDFA      ((=~))

import           Hrange.Types

type NumericRange = (T.Text, T.Text, T.Text, T.Text)
type NumericRangeCandidate = (T.Text, T.Text, T.Text)

-- |Normalizes a range result back into a query. This may not be a minimal
-- compression, but should be shorter in most cases that contain results with
-- repeated elements.
--
-- >>> compress (expand emptyState "n1,n2,n3")
-- "n1..3"
compress :: Result -> Query
compress =
    join .
    map (unionExpr . compressTokens) .
    groupByDomain

  where
    groupByDomain =
      sortBy (\x y -> NS.compare (fst x) (fst y)) .
      M.toList . M.fromListWith (<>) .
      map extractDomain .
      S.toList

    -- snd is a single element list, done so that it can be collapsed easily
    -- into a map later.
    extractDomain :: Query -> (Query, [Query])
    extractDomain s = (T.dropWhile ('.' /=) s, [T.takeWhile ('.' /=) s])

    compressTokens (domain, tokens) = (domain, compressRanges tokens)

    -- Given a list of tokens, return a minimal union expression that can be
    -- embedded inside another token that will expand to the initial list.
    unionExpr ("", tokens)      = join tokens
    unionExpr (domain, [token]) = token <> domain
    unionExpr (domain, tokens)  = "{" <> join tokens <> "}" <> domain

    join = T.intercalate ","

compressRanges :: [Query] -> [Query]
compressRanges =
    sortBy NS.compare .                -- Ensure final output is sorted
    uncurry outputRange .              -- Output final state
    foldl detectRange (Nothing, []) .
    sortBy NS.compare                  -- detectRange maintains state and
                                       -- requires input to be natural sorted.
  where
    detectRange (state, result) token =
      case candidate token of
        -- This token doesn't look like a range. Add it to results, along with
        -- flushing the existing range state.
        Nothing -> (Nothing, token:outputRange state result)

        Just (tokenPrefix, tokenN, tokenSuffix) ->
          let newRangeState = Just (tokenPrefix, tokenSuffix, tokenN, tokenN) in
          case state of
            Nothing ->
              -- Initial case. No current range, and this token may be the
              -- start of a range, so store it in state and don't output any
              -- results.
              (newRangeState, result)

            Just (priorPrefix, priorSuffix, startN, priorN) ->
              if tokenPrefix == priorPrefix &&
                 tokenSuffix == priorSuffix &&
                 asInt tokenN == asInt priorN + 1 &&
                 (
                   T.length tokenN == T.length startN ||
                   zeroCount tokenN == zeroCount startN
                 ) then

                -- This token is a continuation of the current range. Update
                -- the max of the current range, but don't output any results
                -- yet.
                (Just (priorPrefix, priorSuffix, startN, tokenN), result)
              else
                -- Not a continuation of a range, so start a new one
                (newRangeState, outputRange state result)

-- Determine whether a token is potentially part of a numeric range.
candidate :: T.Text -> Maybe NumericRangeCandidate
candidate token = let (prefix, _, _, matches) = matchToken token in
  case matches of
    [n, suffix] -> Just (T.pack prefix, T.pack n, T.pack suffix)
    _           -> Nothing

-- Take the existing range state and output it. Replace the state with
-- the provided newState. The empty token is ignored: this is not a
-- valid query, and is used as a terminal value to ensure all state is
-- flushed.
outputRange :: Maybe NumericRange -> [Query] -> [Query]
outputRange Nothing result = result
outputRange (Just (priorPrefix, priorSuffix, startN, priorN)) result =
    out:result
  where
    out = if startN == priorN then
            priorPrefix <> startN <> priorSuffix
          else
            priorPrefix <> startN <> ".." <> dropZeroes priorN <> priorSuffix

-- This type signature looks weird, but it's the standard way to get match data
-- out of a regex.
matchToken :: T.Text -> (String, String, String, [String])
matchToken token = T.unpack token =~ ("([0-9]+)([^0-9]*)$" :: String)

asInt :: T.Text -> Int
asInt = read . T.unpack

dropZeroes :: T.Text -> T.Text
dropZeroes = T.dropWhile ('0' ==)

zeroCount :: T.Text -> Int
zeroCount = T.length . T.takeWhile ('0' ==)
