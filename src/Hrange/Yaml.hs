module Hrange.Yaml
    ( parseYAML
    , decodeFileWithPath
    , ParserWithState
    ) where

import           Hrange.Parser
import           Hrange.Types

import           Control.DeepSeq      (deepseq)
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.HashMap.Strict  as M
import           Data.Maybe           (fromJust)
import           Data.Monoid          ((<>))
import           Data.Scientific      (Scientific, isInteger, toBoundedInteger)
import qualified Data.Text            as T
import qualified Data.Vector          as V
import qualified Data.Yaml            as Y
import           System.FilePath      (takeBaseName)

type ParserWithState a = ExceptT String (Reader String) a

parseYAML :: Y.Value -> ParserWithState Cluster
parseYAML (Y.Object o) = do
  cluster <- mapM parseKey (M.toList o)
  return . M.fromList $ cluster

parseYAML _ = throwError "YAML top-level object was not an object"

parseKey :: (T.Text, Y.Value) ->
            ParserWithState (Identifier, [Expression])
parseKey (x, exprs) = do
  clusterName <- ask
  parsed <- parseExprs (parseExpr $ parseRange (Just . mkConst $ clusterName)) exprs
  return (x, parsed)

parseExprs :: (T.Text -> ParserWithState Expression) ->
              Y.Value ->
              ParserWithState [Expression]
parseExprs f (Y.Array xs) = concat <$> mapM (parseExprs f) (V.toList xs)
parseExprs f (Y.String x) = replicate 1 <$> f x
parseExprs f (Y.Number x) = replicate 1 <$> f (formatScientific x)
parseExprs f (Y.Bool x)   = replicate 1 <$> f (T.pack . show $ x)
parseExprs _ Y.Null       = return []
parseExprs _ (Y.Object _) = throwError "Nested objects not allowed"

parseExpr :: (T.Text -> ParseResult) ->
             T.Text ->
             ParserWithState Expression
parseExpr f expr =
  case f expr of
    Left _  -> throwError $ "Invalid range expression: " <> T.unpack expr
    Right x -> return x

-- Strict
decodeFileWithPath :: FilePath -> IO (Either String Cluster)
decodeFileWithPath fpath = do
    content <- Y.decodeFileEither fpath
    let ret = case content of
                Left _ -> Nothing
                Right x -> Just x

    return $! parseClusters (fpath, ret)
  where
    parseClusters :: (FilePath, Maybe Y.Value) -> Either String Cluster
    parseClusters (_, Nothing) = Left "Invalid YAML"
    parseClusters (path, Just x) = do
      cluster <- runReader (runExceptT $ parseYAML x) (takeBaseName path) :: Either String Cluster

      return $! cluster `deepseq` cluster

formatScientific :: Scientific -> T.Text
formatScientific x = T.pack $ if isInteger x then
                       show $ fromJust (toBoundedInteger x :: Maybe Int)
                     else
                       show x
