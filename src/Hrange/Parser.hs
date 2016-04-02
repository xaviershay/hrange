{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Hrange.Parser
    ( parseRange
    , ParseResult
    , ParseError
    , commonSuffix
    ) where

import           Hrange.Types

import           Control.Applicative
import Control.Monad.Identity (Identity)
import           Data.List           (transpose)
import qualified Data.Text           as T
import           Text.Parsec         hiding (many, optional, (<|>))
import           Text.Parsec.Expr
import           Text.Printf         (printf)

type RangeParser = ParsecT T.Text (Maybe Expression) Identity Expression
type ParseResult = Either ParseError Expression

-- TODO: Make defaultCluster customizable
defaultCluster :: Expression
defaultCluster = mkConst "GROUPS"

defaultKey :: Expression
defaultKey = mkConst "CLUSTER"

parseRange :: Maybe Expression -> T.Text -> ParseResult
parseRange localCluster input =
  runParser rangeExpr localCluster (T.unpack input) input

rangeExpr :: RangeParser
rangeExpr = outerExpr <* eof

outerExpr :: RangeParser
outerExpr =
      try expr
  <|> innerExpr

expr :: RangeParser
expr = buildExpressionParser table innerExpr <?> "expression"
  where
    table = [ [binary "," Union AssocLeft]
            , [binary "&" Intersection AssocLeft]
            , [binary "-" Difference AssocLeft]
            ]

    binary name fun assoc = Infix (string name >> spaces >> return fun) assoc

innerExpr :: RangeParser
innerExpr = innerExprWithExcludes ""

innerExprCluster :: RangeParser
innerExprCluster = innerExprWithExcludes ":"

innerExprWithExcludes :: String -> RangeParser
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

  where
    constantQ      = mkConst <$> (string "q(" *> many (noneOf ")")  <* char ')')
    constantQuotes = mkConst <$> (string "\"" *> many (noneOf "\"") <* char '"')
    parentheses    = char '(' *> (try outerExpr <|> nothing) <* char ')'
    defaultClusterLookup = functionShortcut '@' $ ClusterLookup defaultCluster
    defaultMemLookup     = functionShortcut '?' $ FunctionMem defaultCluster

-- INNER EXPRESSIONS

clusterLookup :: RangeParser
clusterLookup = ClusterLookup
                  <$> (char '%' *> innerExprCluster)
                  <*> (keysExpr <|> pure defaultKey)
  where
    keysExpr = char ':' *> innerExpr

localClusterLookup :: RangeParser
localClusterLookup = do
  name <- getState

  maybe (fail "no local cluster") (functionShortcut '$' . ClusterLookup) name

functionShortcut :: Char -> (Expression -> Expression) -> RangeParser
functionShortcut c f = f <$> (char c *> innerExprCluster)

function :: RangeParser
function = do
  name  <- many1 alphaNum <* char '('
  exprs <- outerExpr `sepBy` char ';' <* char ')'

  mkFunction name exprs

  where
    mkFunction "has"         = df "has"         2 (\xs -> FunctionHas (xs !! 0) (xs !! 1))
    mkFunction "mem"         = df "mem"         2 (\xs -> FunctionMem (xs !! 0) (xs !! 1))
    mkFunction "clusters"    = df "clusters"    1 (FunctionHas defaultKey . head)
    mkFunction "allclusters" = df "allclusters" 0 (const FunctionAllClusters)
    mkFunction name          = const . fail $ printf "Unknown function: %s" (name :: String)

    df name expected f exprs =
      if length exprs == expected then
        return $ f exprs
      else
        fail $ printf "%s() expects %i arguments, got %i" (name :: String) expected (length exprs)

clustersFunction :: RangeParser
clustersFunction = FunctionHas defaultKey <$> (char '*' *> innerExpr)

-- IDENTIFIERS

-- TODO: Allow escaping?
regex :: RangeParser
regex = do
  source <- char '/' *> many (noneOf "/") <* char '/'

  case makeShowableRegex source of
    Just rx -> return rx
    Nothing -> fail ("Invalid regex: " ++ source)

nothing :: RangeParser
nothing = return $ Product []

productExpr :: String -> RangeParser
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

  where
    productBraces = char '{' *> (try outerExpr <|> nothing) <* char '}'

numericRange :: RangeParser
numericRange = do
  prefix  <- many letter
  bottom  <- many1 digit
  _       <- string ".."
  prefix2 <- many letter
  top     <- many1 digit

  if prefix2 == commonSuffix [prefix, prefix2] then
    let diff    = length bottom - length top in
    let prefix' = prefix ++ take diff bottom in
    let bottom' = drop diff bottom in
    -- TODO: Quickcheck to verify read here is safe
    return $ NumericRange (T.pack prefix') (length bottom') (read bottom') (read top)
  else
    fail "Second prefix in range must be common to first prefix"

identifier :: String -> RangeParser
identifier excludes = mkConst <$> many1 (alphaNum <|> oneOf punctuation)
  where
    punctuation = stripChars excludes "-_:."

-- HELPER METHODS

commonSuffix :: [String] -> String
commonSuffix xs =
  reverse . map head . takeWhile charIsSame . transpose . map reverse $ xs
  where
    l = length xs - 1

    charIsSame :: String -> Bool
    charIsSame []     = False
    charIsSame (c:cs) = length cs == l && all (== c) cs

-- http://www.rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string#Haskell
stripChars :: String -> String -> String
stripChars = filter . flip notElem

