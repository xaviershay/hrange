import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath.Find

import Data.Maybe
import Data.Either

import qualified Data.Text as T
import Text.Parsec
import Text.Show.Pretty

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import qualified Data.Yaml as Y

import Lib
import Parser

import System.Environment (lookupEnv)

type RangeSpec = M.Map String [S.Set String]
type RawCluster = M.Map T.Text Y.Value

-- TODO: Error on spec parse failure
-- TODO: Warn when RANGE_SPEC_PATH not set
main :: IO ()
main = do
  specPath <- lookupEnv "RANGE_SPEC_PATH"
  specs <- maybe (return []) (\x -> find always (extension ==? ".spec") (x ++ "/spec/expand")) specPath
  yamls <- maybe (return []) (\x -> find always (extension ==? ".yaml") (x ++ "/spec/expand")) specPath

  -- TODO: Load YAML files also
  contents <- mapM readFile specs
  clusters <- mapM (\x -> Y.decodeFile x :: IO (Maybe RawCluster)) yamls
  let parsedClusters = zip yamls (map parseCluster clusters)

  putStrLn $ ppShow parsedClusters
  -- TODO: include keys in specs, map keys to directory, merge specs + clusters
  -- into single data structure.

  --defaultMain (tests $ rights (zipWith (curry parseSpec) specs contents))

normalizeYaml :: Y.Value -> [String]
normalizeYaml (Y.Array xs) = map yamlToString $ V.toList xs
normalizeYaml x = [yamlToString x]

-- TODO: Error handling
yamlToString :: Y.Value -> String
yamlToString (Y.String x) = show x
yamlToString (Y.Number x) = show x

parseCluster :: Maybe RawCluster -> Either String Cluster
parseCluster Nothing = Left "Could not read or parse as YAML"
parseCluster (Just xs) = if null errors then
                           Right $ M.map (S.fromList . rights) parsedMap
                         else
                           Left . unlines $ map show errors

  where
    errors = concatMap lefts $ M.elems parsedMap
    parsedMap = M.map (\x -> map parseRange (normalizeYaml x)) xs

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

tests specs = testGroup "Range Spec" $ map rangeSpecs specs

rangeSpecs (name, specs) = testGroup name $ map specTest specs

specTest (expr, expected) =
  testCaseSteps ("Evaluating \"" ++ expr ++ "\"") $ \step -> do
    step "Parsing"
    assert $ isRight actual

    step "Evaluating"
    expected @=? results 
  where
    actual = parseRange expr
    fromRight (Right r) = r
    results = runEval emptyState $ eval (fromRight actual)
