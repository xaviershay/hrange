module Parser
    ( module Parser
    ) where


import Lib

import Text.Parsec
import Data.Text as T
import Data.Maybe

-- TODO: Delete all this and start again with range-spec
-- This is all just a sketch to learn parsec

constantBraces = do
  lhs <- char '{'
  expr <- rangeExpression
  rhs <- char '}'

  return expr

product = do
  exprs <- many1 (try constant <|> constantBraces)
  return $
    -- This conditional isn't strictly required, but makes reading parse trees
    -- much easier.  Potentially consider moving optimizations into another
    -- parse, though I'm not sure what other optimization would be useful
    -- (reorderings?)
    if Prelude.length exprs == 1 then
      Prelude.head exprs
    else
      Product exprs

constant = do
  ident <- many1 $ (alphaNum <|> char '-')
  return $ Const (pack ident)

keys = do
  _ <- char ':'
  subExpression

clusterLookup = do
  first <- char '%'
  names <- subExpression
  keys  <- optionMaybe keys

  return $ ClusterLookup names (fromMaybe (Const (T.pack "CLUSTER")) keys)

union = do
  lhs <- subExpression
  _   <- many $ char ' '
  sep <- char ','
  _   <- many $ char ' '
  rhs <- rangeExpression

  return $ Union lhs rhs

intersection = do
  lhs <- subExpression
  _   <- many $ char ' '
  sep <- char '&'
  _   <- many $ char ' '
  rhs <- rangeExpression

  return $ Intersection lhs rhs

difference = do
  lhs <- subExpression
  _   <- many $ char ' '
  sep <- char '-'
  _   <- many $ char ' '
  rhs <- rangeExpression

  return $ Difference lhs rhs

regex = do
  _ <- char '/'
  (Const x) <- constant -- TODO: This is messy, refactor
  _ <- char '/'

  return $ Regexp x

subExpression = do
  expr <- clusterLookup <|> Parser.product
  _    <- optionMaybe eof
  return expr

rangeExpression = do
  expr <- try union <|> try intersection <|> try difference <|> regex <|> subExpression

  return expr

outerExpression = do
  expr <- rangeExpression
  _    <- eof

  return expr

parseRange :: String -> Either ParseError Expression
parseRange input = parse outerExpression input input
