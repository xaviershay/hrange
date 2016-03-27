{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- Disable top level signature warning, since the parser types aren't that
-- enlightening.
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Hrange.Parser
    ( parseRange
    , ParseResult
    , ParseError
    ) where

import           Hrange.Types

import           Control.Applicative hiding (Const)
import           Control.Monad       (guard)
import           Data.List           (transpose)
import           Data.Maybe          (fromJust, fromMaybe, isJust)
import qualified Data.Text           as T
-- Hide a few names that are provided by Applicative.
import           Text.Parsec         hiding (many, optional, (<|>))
import           Text.Parsec.Expr
import           Text.Printf         (printf)

type ParseResult = Either ParseError Expression

parseRange :: Maybe Expression -> String -> ParseResult
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

binary name fun assoc = Infix (do{ _ <- string name; spaces; return fun }) assoc

innerExpr = innerExprWithExcludes ""
innerExprCluster = innerExprWithExcludes ":"
innerExprWithExcludes excludes =
  (   clusterLookup
  <|> localClusterLookup
  <|> defaultClusterLookup
  <|> defaultMemLookup
  <|> clustersFunction
  <|> try parentheses
  <|> regex
  <|> try constantQ
  <|> try function
  <|> constantQuotes
  <|> productExpr excludes
  ) <* spaces

-- OUTER EXPRESSIONS

parentheses = char '(' *> (try outerExpr <|> nothing) <* char ')'

-- INNER EXPRESSIONS

clusterLookup = do
  _     <- char '%'
  names <- innerExprCluster
  keys  <- optionMaybe keysExpr

  return $ ClusterLookup names (fromMaybe (mkConst "CLUSTER") keys)

localClusterLookup = do
  name <- getState
  guard $ isJust name

  _     <- char '$'
  keys  <- innerExprCluster

  return $ ClusterLookup (fromJust name) keys

-- TODO: Make GROUPS customizable
defaultClusterLookup = do
  _  <- char '@'
  xs <- innerExprCluster

  return $ ClusterLookup (mkConst "GROUPS") xs

defaultMemLookup = do
  _  <- char '?'
  xs <- innerExprCluster

  return $ FunctionMem (mkConst "GROUPS") xs

keysExpr = char ':' *> innerExpr

function = do
  name  <- many1 alphaNum <* char '('
  exprs <- outerExpr `sepBy` char ';' <* char ')'

  mkFunction name exprs

  where
    mkFunction "has" exprs = defineFunction "has" 2 (\xs -> FunctionHas (xs !! 0) (xs !! 1)) exprs
    mkFunction "mem" exprs = defineFunction "mem" 2 (\xs -> FunctionMem (xs !! 0) (xs !! 1)) exprs
    mkFunction "clusters" exprs = defineFunction "clusters" 1 (FunctionClusters . head) exprs
    mkFunction "allclusters" exprs = defineFunction "allclusters" 0 (const FunctionAllClusters) exprs
    mkFunction name _ = fail $ printf "Unknown function: %s" (name :: String)

    defineFunction name expected f exprs =
      if length exprs == expected then
        return $ f exprs
      else
        fail $ printf "%s() expects %i arguments, got %i" (name :: String) expected (length exprs)

clustersFunction = mkClusters <$> (char '*' *> innerExpr)
  where
    mkClusters = FunctionClusters

-- IDENTIFIERS

-- TODO: Allow escaping?
regex = do
  source <- char '/' *> many (noneOf "/") <* char '/'

  case makeShowableRegex source of
    Just rx -> return rx
    Nothing -> fail ("Invalid regex: " ++ source)

constantQ      = mkConst <$> (string "q(" *> many (noneOf ")") <* char ')')
constantQuotes = mkConst <$> (string "\"" *> many (noneOf "\"") <* char '"')

nothing = return $ Product []

productExpr excludes = do
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
  bottom   <- many1 digit
  _       <- string ".."
  prefix2 <- many letter
  top   <- many1 digit

  let commonPrefix = commonSuffix [prefix, prefix2]
  if commonPrefix == prefix2 then
    let diff    = length bottom - length top in
    let prefix' = prefix ++ take diff bottom in
    let bottom'  = drop diff bottom in
    -- TODO: Quickcheck to verify read here is safe
    return $ NumericRange (T.pack prefix') (length bottom') (read bottom') (read top)
  else
    fail "Second prefix in range must be common to first prefix"

-- TODO: quick check and stuff
commonSuffix :: [String] -> String
commonSuffix xs = map head . takeWhile (\(c:cs) -> length cs == l && all (== c) cs) . transpose $ xs
  where l = length xs - 1

productBraces = char '{' *> (try outerExpr <|> nothing) <* char '}'

-- http://www.rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string#Haskell
stripChars :: String -> String -> String
stripChars = filter . flip notElem

-- TODO: More chars here maybe
identifier excludes = mkConst <$> many1 (alphaNum <|> oneOf punctuation)
  where
    punctuation = stripChars excludes "-_:."
