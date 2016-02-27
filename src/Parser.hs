{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser
    ( module Parser
    ) where

import Lib

import qualified Data.Text as T
import Data.List (transpose)
import Data.Maybe (fromMaybe)

import Control.Applicative hiding (Const)
-- Hide a few names that are provided by Applicative.
import Text.Parsec hiding (many, optional, (<|>))

parseRange :: String -> Either ParseError Expression
parseRange input = parse rangeExpr input input

rangeExpr = outerExpr <* eof

outerExpr =
      try (joiner Union ',')
  <|> try (joiner Intersection '&')
  <|> try (joiner Difference '-')
  <|> innerExpr

innerExpr = innerExprWithExcludes ""
innerExprCluster = innerExprWithExcludes ":"
innerExprWithExcludes excludes =
  (   clusterLookup
  <|> clustersFunction
  <|> try parentheses
  <|> regex
  <|> constantQ
  <|> try function
  <|> constantQuotes
  <|> Parser.product excludes
  ) <* spaces

-- OUTER EXPRESSIONS

parentheses = char '(' *> outerExpr <* char ')'

joiner t c  = t <$> innerExpr <* spaces <* char c <* spaces <*> outerExpr

-- INNER EXPRESSIONS

clusterLookup = do
  first <- char '%'
  names <- innerExprCluster
  keys  <- optionMaybe keys

  return $ ClusterLookup names (fromMaybe (Const "CLUSTER") keys)

keys = char ':' *> innerExpr

function = mkFunction <$>
  try (many1 alphaNum) <* char '(' <*>
  outerExpr `sepBy` char ';' <* char ')'

  where
    mkFunction n = Function (T.pack n)

clustersFunction = mkClusters <$> (char '*' *> innerExpr)
  where
    mkClusters expr = Function (T.pack "clusters") [expr]

-- IDENTIFIERS

-- TODO: Allow escaping?
-- TODO: Actual regex rather than packing to identifier
regex = Regexp . T.pack <$> (char '/' *> many (noneOf "/") <* char '/')
constantQ      = Const . T.pack <$> (string "q(" *> many (noneOf ")") <* char ')')
constantQuotes = Const . T.pack <$> (string "\"" *> many (noneOf "\"") <* char '"')

product excludes = do
  exprs <- many1 (try numericRange <|> try (identifier excludes) <|> productBraces)
  return $
    -- This conditional isn't strictly required, but makes reading parse trees
    -- much easier.  Potentially consider moving optimizations into another
    -- parse, though I'm not sure what other optimization would be useful
    -- (reorderings?)
    if Prelude.length exprs == 1 then
      Prelude.head exprs
    else
      Product exprs

numericRange = mkRange <$> many letter <*> many1 digit <* string ".." <*> many letter <*> many1 digit
  where
    fromConst (Const x) = x
    mkRange prefix lower prefix2 upper =
      let commonPrefix = commonSuffix [prefix, prefix2] in

      -- TODO: Ensure len lower <= len upper by rebalancing into prefix. Then cast
      -- to int.
      NumericRange (T.pack commonPrefix) (T.pack lower) (T.pack upper)


-- TODO: quick check and stuff
commonSuffix :: [String] -> String
commonSuffix xs = map head . takeWhile (\(c:cs) -> (length cs) == l && all (== c) cs) . transpose $ xs
  where l = length xs - 1

productBraces = char '{' *> outerExpr <* char '}'

-- http://www.rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string#Haskell
stripChars :: String -> String -> String
stripChars = filter . flip notElem

-- TODO: More chars here maybe
identifier excludes = Const . T.pack <$> many1 (alphaNum <|> oneOf punctuation)
  where
    punctuation = stripChars excludes "-_:."
