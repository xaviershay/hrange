module Parser
    ( module Parser
    ) where


import Lib

import Text.Parsec
import qualified Data.Text as T
import Data.List (transpose)
import Data.Maybe

parseRange :: String -> Either ParseError Expression
parseRange input = parse rangeExpr input input

rangeExpr = do
  expr <- outerExpr
  _ <- eof

  return expr

outerExpr = do
  expr <- try union <|> try difference <|> try intersection <|> innerExpr

  return expr

innerExpr = do
  expr <- clusterLookup <|> clustersFunction <|> try parentheses <|> regex <|> constantQ <|> try function <|> constantQuotes <|> Parser.product
  _ <- spaces

  return expr

-- OUTER EXPRESSIONS

parentheses = do
  _    <- char '('
  expr <- outerExpr
  _    <- char ')'

  return expr

union = do
  lhs <- innerExpr
  _   <- many $ char ' '
  sep <- char ','
  _   <- many $ char ' '
  rhs <- outerExpr

  return $ Union lhs rhs

intersection = do
  lhs <- innerExpr
  _   <- many $ char ' '
  sep <- char '&'
  _   <- many $ char ' '
  rhs <- outerExpr

  return $ Intersection lhs rhs

difference = do
  lhs <- innerExpr
  _   <- many $ char ' '
  sep <- char '-'
  _   <- many $ char ' '
  rhs <- outerExpr

  return $ Difference lhs rhs

-- INNER EXPRESSIONS

clusterLookup = do
  first <- char '%'
  names <- innerExpr
  keys  <- optionMaybe keys

  return $ ClusterLookup names (fromMaybe (Const (T.pack "CLUSTER")) keys)

keys = do
  _ <- char ':'
  innerExpr

function = do
  name <- try (many1 alphaNum)
  _    <- char '('
  args <- outerExpr `sepBy` char ';'
  _    <- char ')'

  return $ Function (T.pack name) args

clustersFunction = do
  _ <- char '*'
  expr <- innerExpr

  return $ Function (T.pack "clusters") [expr]

-- IDENTIFIERS

regex = do
  _ <- char '/'
  r <- many (noneOf "/") -- TODO: Allow escaping?
  _ <- char '/'

  -- TODO: Pull in a regex library and use native type here
  return $ Regexp (T.pack r)

constantQ = do
  _ <- string "q("
  q <- many (noneOf ")") -- TODO: Is this right?
  _ <- char ')'

  return $ Const (T.pack q)

constantQuotes = do
  _ <- char '"'
  q <- many (noneOf "\"") -- TODO: Is this right?
  _ <- char '"'

  return $ Const (T.pack q)

product = do
  exprs <- many1 (try numericRange <|> try identifier <|> productBraces)
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

  -- TODO: Ensure len lower <= len upper by rebalancing into prefix. Then cast
  -- to int.
  return $ NumericRange
    (T.pack commonPrefix)
    (T.pack lower)
    (T.pack upper)

  where
    fromConst (Const x) = x


-- TODO: quick check and stuff
commonSuffix :: [String] -> String
commonSuffix xs = map head . takeWhile (\(c:cs) -> (length cs) == l && all (== c) cs) . transpose $ xs
  where l = length xs - 1

productBraces = do
  lhs  <- char '{'
  expr <- outerExpr
  rhs  <- char '}'

  return expr

identifier = do
  ident <- many1 $ alphaNum <|> oneOf "-_:."
  return $ Const (T.pack ident)
