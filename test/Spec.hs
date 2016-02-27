{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath.Find

import Data.Maybe
import Data.Either

import qualified Data.Text as T
import Text.Parsec
import Text.Show.Pretty

import System.FilePath (takeDirectory, takeBaseName)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Vector as V

import qualified Data.Yaml as Y

import Lib
import Parser

import System.Environment (lookupEnv)

type RangeSpec = M.Map String [S.Set String]

type RawCluster = M.Map T.Text [T.Text]

instance Y.FromJSON RawCluster where
  parseJSON (Y.Object o) = mapM parseKey (M.fromList . HM.toList $ o)
  parseJSON invalid = fail "YAML top-level object was not an object"

parseKey :: Y.Value -> Y.Parser [T.Text]
parseKey (Y.String x) = return [x]
parseKey (Y.Number x) = return [T.pack . show $ x]
parseKey (Y.Bool x)   = return [T.pack . show $ x]
parseKey (Y.Null)     = return []
parseKey (Y.Object _) = fail "Nested objects not allowed"
parseKey (Y.Array xs) = concat <$> mapM parseKey (V.toList xs)


-- TODO: Error on spec parse failure
-- TODO: Warn when RANGE_SPEC_PATH not set
main :: IO ()
main = do
  specPath <- lookupEnv "RANGE_SPEC_PATH"
  specs <- maybe (return []) (\x -> find always (extension ==? ".spec") (x ++ "/spec/expand")) specPath
  yamls <- maybe (return []) (\x -> find always (extension ==? ".yaml") (x ++ "/spec/expand")) specPath

  -- TODO: Load YAML files also
  contents <- mapM readFile specs
  clusters <- mapM Y.decodeFile yamls
  -- TODO: Error on bad clusters
  let parsedClusters = zip yamls (map parseCluster clusters)

  let parsedClusters' = (M.fromListWith M.union (map (\(k, v) -> (takeDirectory k, M.singleton (T.pack . takeBaseName $ k) (fromRight v))) parsedClusters))

  let parsedSpecs = rights (zipWith (curry parseSpec) specs contents)
  --putStrLn $ ppShow parsedSpecs
  -- TODO: include keys in specs, map keys to directory, merge specs + clusters
  -- into single data structure.

  defaultMain (tests parsedSpecs parsedClusters')

fromRight (Right x) = x

parseCluster :: Maybe RawCluster -> Either String Cluster
parseCluster Nothing = Left "Could not read or parse as YAML"
parseCluster (Just xs) = if null errors then
                           Right $ M.map (S.fromList . rights) parsedMap
                         else
                           Left . unlines $ map show errors

  where
    errors = concatMap lefts $ M.elems parsedMap
    parsedMap = M.map (\x -> map (parseRange . show) x) xs

eol = char '\n'

comment = do
  _ <- char '#'
  _ <- line

  return Nothing

line = do
  result <- many1 (noneOf "\n")
  _      <- optionMaybe eol
  return $ Just result

rangeSingleSpec = do
  _ <- many comment
  expr <- line
  spec <- many (comment <|> line)
  _    <- optionMaybe eol

  return (fromJust expr, S.fromList . map T.pack . catMaybes $ spec)

rangeSpec = many rangeSingleSpec

parseSpec :: (String, String) -> Either ParseError (String, [(String, Result)])
parseSpec (name, input) = f p
  where
    p = parse rangeSpec name input
    f (Right parse) = Right (name, parse)
    f (Left err)    = Left err

tests specs clusters = testGroup "Range Spec" $ map (rangeSpecs clusters) specs

rangeSpecs clusters (name, specs) =
  testGroup (takeBaseName name) $ map (specTest state) specs
  where
    state = fromMap $ M.findWithDefault M.empty (takeDirectory name) clusters

specTest state (expr, expected) =
  testCaseSteps ("Evaluating \"" ++ expr ++ "\"") $ \step -> do
    step "Parsing"
    assert $ isRight actual

    step "Evaluating"
    expected @=? results
  where
    actual = parseRange expr
    fromRight (Right r) = r
    results = runEval state $ eval (fromRight actual)
