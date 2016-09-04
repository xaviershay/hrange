{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- Code to take a query string and turn it into an AST.
--
-- This module makes heavy use of parsec combinators and applicative operators
-- (<$>, <*>, <*, *>).
--
-- http://jakewheat.github.io/intro_to_parsing/ is a good introduction to these
-- concepts. Section 4 contains an explanation of the applicative operators.

module Hrange.Parser
    ( parseRange
    , ParseResult
    , ParseError
    ) where

import           Hrange.Text
import           Hrange.Types

import           Control.Applicative
import           Control.Monad.Identity (Identity)
import           Data.Char              (isDigit)
import           Data.List              (tails)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Data.Text.Format
import qualified Data.Text.Lazy         as TL
import           Text.Parsec            hiding (many, optional, (<|>))
import           Text.Parsec.Expr
import           Text.Printf            (printf)
import           Text.Read              (readMaybe)

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

    binary name fun = Infix (string name >> spaces >> return fun)

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
  <|> function
  <|> constantQuotes
  <|> productExpr excludes
  ) <* spaces

  where
    constantQ      = mkConst <$> (string "q(" *> many (noneOf ")")  <* closing ')')
    constantQuotes = mkConst <$> (string "\"" *> many (noneOf "\"") <* closing '"')
    parentheses    = char '(' *> (nothing <* closing ')' <|> (outerExpr <?> "expression") <* closing ')')
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
  ex    <- char '$' *> innerExprCluster
  state <- getState

  maybe (fail "no local cluster") (\x -> return . ClusterLookup x $ ex) state

functionShortcut :: Char -> (Expression -> Expression) -> RangeParser
functionShortcut c f = f <$> (char c *> innerExprCluster)

closing :: Stream s m Char => Char -> ParsecT s u m Char
closing c = char c <?> "closing " <> [c]

function :: RangeParser
function = do
  name  <- try (many1 alphaNum <* char '(')
  exprs <- ((outerExpr <?> "expression") `sepBy` char ';') <* closing ')'

  mkFunction name exprs

  where
    mkFunction "has"         = df "has"         2 (\[ks, ns] -> FunctionHas ks ns)
    mkFunction "mem"         = df "mem"         2 (\[cs, ns] -> FunctionMem cs ns)
    mkFunction "clusters"    = df "clusters"    1 (FunctionHas defaultKey . head)
    mkFunction "allclusters" = df "allclusters" 0 (const FunctionAllClusters)
    mkFunction name          = const . fail $ printf "Unknown function: %s" (name :: String)

    df :: (Foldable t, Monad m) => String -> Int -> (t a -> r) -> t a -> m r
    df name expected f exprs =
      if length exprs == expected then
        return . f $ exprs
      else
        fail . TL.unpack $ format "{}() expecting {} arguments, got {}"
                             (name, expected, length exprs)

clustersFunction :: RangeParser
clustersFunction = FunctionHas defaultKey <$> (char '*' *> innerExpr)

-- IDENTIFIERS

-- TODO: Allow escaping?
regex :: RangeParser
regex = do
  source <- char '/' *> many (noneOf "/") <* char '/'

  maybe (fail $ "Invalid regex: " ++ source) return (makeShowableRegex source)

nothing :: RangeParser
nothing = return . Product $ []

productExpr :: String -> RangeParser
productExpr excludes = unwrap
  <$> many1 (try numericRange <|> try (identifier excludes) <|> productBraces)

  where
    -- This unwrap isn't required, but makes reading parse trees much easier.
    unwrap [x] = x
    unwrap xs  = Product xs

    productBraces = char '{' *> (try outerExpr <|> nothing) <* char '}'

numericRange :: RangeParser
numericRange = do
  (prefixCandidate, bottomCandidate)  <- parseBottom
  _                                   <- string ".."
  (prefix2, top)                      <- choice (map parseTopPrefix . tails $
                                                 prefixCandidate)

  if prefix2 == commonSuffix [prefixCandidate, prefix2] then
    let diff   = length bottomCandidate - length top in
    let prefix = prefixCandidate ++ take diff bottomCandidate in
    let bottom = drop diff bottomCandidate in
    let result = NumericRange (T.pack prefix) (length bottom)
                   <$> readMaybe bottom
                   <*> readMaybe top
                   in
    maybe (fail "Bottom or top were not ints") return result
  else
    fail "Second prefix in range must be common to first prefix"

  where
    parseBottom = do
      x <- many1 (letter <|> digit)

      let bottom = rtakeWhile isDigit x
      let prefix = rdrop (length bottom) x

      if bottom == "" then
        fail "No digit at right of prefix"
      else
        return (prefix, bottom)

    rtakeWhile :: (a -> Bool) -> [a] -> [a]
    rtakeWhile f = reverse . takeWhile f . reverse

    rdrop :: Int -> [a] -> [a]
    rdrop n xs = take (length xs - n) xs

    parseTopPrefix prefix = do
      p   <- try (string prefix)
      top <- many1 digit

      return (p, top)

identifier :: String -> RangeParser
identifier excludes = mkConst <$> many1 (alphaNum <|> oneOf punctuation)
  where
    punctuation = stripChars excludes "-_:."
