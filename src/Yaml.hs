module Yaml
    ( parseYAML
    ) where

import Types
import Parser
import qualified Data.Yaml              as Y
import qualified Data.HashMap.Strict    as M
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Maybe (fromJust)
import           Data.Scientific        (Scientific, isInteger,
                                         toBoundedInteger)

type ParserWithState a = ExceptT String (Reader String) a

parseYAML :: Y.Value -> ParserWithState Cluster
parseYAML (Y.Object o) = do
  cluster <- mapM parseKey (M.toList o)
  return . M.fromList $ cluster

parseYAML invalid = throwError "YAML top-level object was not an object"

parseKey :: (T.Text, Y.Value) ->
            ParserWithState (Identifier2, [Expression])
parseKey (x, exprs) = do
  clusterName <- ask
  parsed <- parseExprs (parseExpr $ parseRange (Just . mkConst $ clusterName)) exprs
  return $ (x, parsed)

parseExprs :: (String -> ParserWithState Expression) ->
              Y.Value ->
              ParserWithState [Expression]
parseExprs f (Y.Array xs) = concat <$> mapM (parseExprs f) (V.toList xs)
parseExprs f (Y.String x) = replicate 1 <$> f (T.unpack x)
parseExprs f (Y.Number x) = replicate 1 <$> f (formatScientific x)
parseExprs f (Y.Bool x)   = replicate 1 <$> f (show x)
parseExprs f Y.Null       = return []
parseExprs f (Y.Object _) = throwError "Nested objects not allowed"

parseExpr :: (String -> ParseResult) ->
             String ->
             ParserWithState Expression
parseExpr f expr =
  case f expr of
    Left err  -> throwError $ "Invalid range expression: " ++ expr
    Right exp -> return exp

formatScientific :: Scientific -> String
formatScientific x = if isInteger x then
                       show $ fromJust (toBoundedInteger x :: Maybe Int)
                     else
                       show x
