{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import System.FilePath.Find

import Debug.Trace

import Data.Maybe
import Data.Either

import qualified Data.Text as T
import Text.Parsec
import Text.Show.Pretty
import qualified Text.Regex.TDFA as R

import System.FilePath (takeDirectory, takeBaseName)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import Data.Scientific (toBoundedInteger, isInteger, Scientific)

import Control.Monad (replicateM)
import Lib
import Parser

import System.Environment (lookupEnv)


type RangeSpec = M.HashMap String [S.HashSet String]

type RawCluster = M.HashMap ClusterKey [T.Text]

instance Y.FromJSON RawCluster where
  parseJSON (Y.Object o) = do
    cluster <- mapM parseKey (M.toList o)
    return . M.fromList $ cluster

  parseJSON invalid = fail "YAML top-level object was not an object"

parseKey (x, exprs) = do
  parsed <- parseExprs exprs
  return $ (ClusterKey x, parsed)

parseExprs :: Y.Value -> Y.Parser [T.Text]
parseExprs (Y.String x) = return [x]
parseExprs (Y.Number x) = return [T.pack . formatScientific $ x]
parseExprs (Y.Bool x)   = return [T.pack . show $ x]
parseExprs (Y.Object _) = fail "Nested objects not allowed"
parseExprs (Y.Array xs) = concat <$> mapM parseExprs (V.toList xs)
parseExprs Y.Null       = return []

formatScientific :: Scientific -> String
formatScientific x = if isInteger x then
                       show $ fromJust (toBoundedInteger x :: Maybe Int)
                     else
                       show x

-- TODO: Error on spec parse failure
-- TODO: Warn when RANGE_SPEC_PATH not set
main :: IO ()
main = do
  specPath <- lookupEnv "RANGE_SPEC_PATH"
  specs <- maybe (return []) (\x -> find always (extension ==? ".spec") (x ++ "/spec/expand")) specPath
  yamls <- maybe (return []) (\x -> find always (extension ==? ".yaml") (x ++ "/spec/expand")) specPath

  -- TODO: Load YAML files also
  contents <- mapM readFile specs
  clusters <- mapM decodeFileWithPath yamls
  -- TODO: Error on bad clusters
  -- TODO: This is a mess
  let parsedClusters = map (\(fp, c) -> (fp, parseCluster (fp, c))) clusters

  let parsedClusters' = M.fromListWith M.union (map (\(k, v) -> (takeDirectory k, M.singleton (T.pack . takeBaseName $ k) (fromRight v))) parsedClusters)

  let parsedSpecs = rights (zipWith (curry parseSpec) specs contents)
  --putStrLn $ ppShow parsedSpecs
  -- TODO: include keys in specs, map keys to directory, merge specs + clusters
  -- into single data structure.

  defaultMain (tests parsedSpecs parsedClusters')

decodeFileWithPath path = do
    content <- Y.decodeFile path
    return (path, content)

fromRight (Right x) = x

parseCluster :: (FilePath, Maybe RawCluster) -> Either String Cluster
parseCluster (_, Nothing) = Left "Could not read or parse as YAML"
parseCluster (fp, Just xs) = if null errors then
                               Right $ M.map rights parsedMap
                             else
                               Left . unlines $ map show errors

  where
    errors = concatMap lefts $ M.elems parsedMap
    clusterName = Just . Const . T.pack $ takeBaseName fp
    parsedMap = M.map (map $ parseRange clusterName . T.unpack) xs

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

tests specs clusters = testGroup ""
  [ testGroup "Range Spec" $ map (rangeSpecs clusters) specs
  , testGroup "Quickchecks" quickchecks
  ]

instance Arbitrary Expression where
  arbitrary = frequency
    [ (1, Const <$> printable)
    , (1, oneof
            [ Intersection  <$> arbitrary <*> arbitrary
            , Union         <$> arbitrary <*> arbitrary
            , Difference    <$> arbitrary <*> arbitrary
            , ClusterLookup <$> arbitrary <*> arbitrary
            , FunctionHas   <$> arbitrary <*> arbitrary
            , FunctionClusters <$> arbitrary
            , fromJust . makeShowableRegex <$> scale ((`mod` 10) . abs) (listOf1 $ elements ['a'..'z'])
            , pure FunctionAllClusters
            , Product <$> scale ((`mod` 10) . abs) arbitrary
            , NumericRange <$> printable <*> elements [0..10] <*> smallInt <*> smallInt
            ])
    ]

smallInt = elements [0..10]

printable = T.pack <$> listOf1 (elements ['a'..'z'])

quickchecks =
  [ QC.testProperty "eval is fully defined" $
      \expr -> runEval emptyState (eval expr) `seq` True
  ]

rangeSpecs clusters (name, specs) =
  testGroup (takeBaseName name) $ map (specTest state) specs
  where
    state = fromMap $ M.lookupDefault M.empty (takeDirectory name) clusters

specTest state (expr, expected) =
  testCaseSteps ("Evaluating \"" ++ expr ++ "\"") $ \step -> do
    step "Parsing"
    assert $ isRight actual

    step $ "Evaluating " ++ show (fromRight actual)
    expected @=? results
  where
    actual = parseRange Nothing expr
    fromRight (Right r) = r
    results = runEval state $ eval (fromRight actual)
