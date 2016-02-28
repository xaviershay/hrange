{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser
    ( module Parser
    ) where

import Lib

import qualified Data.Text as T
import Data.List (transpose)
import Data.Maybe (fromMaybe, isJust, fromJust)

import Control.Applicative hiding (Const)
import Control.Monad (guard)
-- Hide a few names that are provided by Applicative.
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.Expr

parseRange :: Maybe Expression -> String -> Either ParseError Expression
parseRange localCluster input = runParser rangeExpr localCluster input input

rangeExpr = outerExpr <* eof

outerExpr =
      try expr
  <|> innerExpr

expr    = buildExpressionParser table innerExpr
         <?> "expression"

table = [ [binary "," Union AssocLeft]
        , [binary "&" Intersection AssocLeft]
        , [binary "-" Difference AssocLeft]
        ]

binary name fun assoc = Infix (do{ string name; spaces; return fun }) assoc

innerExpr = innerExprWithExcludes ""
innerExprCluster = innerExprWithExcludes ":"
innerExprWithExcludes excludes =
  (   clusterLookup
  <|> localClusterLookup
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

localClusterLookup = do
  name <- getState
  guard $ isJust name

  first <- char '$'
  keys  <- innerExprCluster

  return $ ClusterLookup (fromJust name) keys

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

numericRange = do
  prefix  <- many letter
  lower   <- many1 digit
  _       <- string ".."
  prefix2 <- many letter
  upper   <- many1 digit

  let commonPrefix = commonSuffix [prefix, prefix2]
  if commonPrefix == prefix2 then
    let diff    = length lower - length upper in
    let prefix' = prefix ++ (take diff lower) in
    let lower'  = drop diff lower in
    -- TODO: Quickcheck to verify read here is safe
    return $ NumericRange (T.pack prefix') (length lower') (read lower') (read upper)
  else
    fail "Second prefix in range must be common to first prefix"

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
