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
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Lib
import Parser

import System.Environment (lookupEnv)


type RangeSpec = M.HashMap String [S.HashSet String]
type MyParser a = ExceptT String (Reader T.Text) a

parseYAML :: Y.Value -> MyParser Cluster
parseYAML (Y.Object o) = do
  cluster <- mapM parseKey (M.toList o)
  return . M.fromList $ cluster

parseYAML invalid = fail "YAML top-level object was not an object"

parseKey :: (T.Text, Y.Value) -> MyParser (Identifier PostEval, [Expression])
parseKey (x, exprs) = do
  parsed <- parseExprs (parseExpr $ parseRange (Just . mkConst . T.unpack $ x)) exprs
  return $ (Identifier x, parsed)

parseExprs :: (String -> MyParser Expression) -> Y.Value -> MyParser [Expression]
parseExprs f (Y.Array xs) = concat <$> mapM (parseExprs f) (V.toList xs)
parseExprs f (Y.String x) = replicate 1 <$> f (T.unpack x)
parseExprs f (Y.Number x) = replicate 1 <$> f (formatScientific x)
parseExprs f (Y.Bool x)   = replicate 1 <$> f (show x)
parseExprs f Y.Null       = return []
parseExprs f (Y.Object _) = fail "Nested objects not allowed"

parseExpr :: (String -> ParseResult) -> String -> MyParser Expression
parseExpr f expr =
  case f expr of
    Left err  -> fail $ "Invalid range expression: " ++ expr
    Right exp -> return exp

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
  raw <- mapM decodeFileWithPath yamls
  let clusters = map (\(path, Just x) -> (path, Just . fromRight $ runReader (runExceptT (parseYAML x)) "hi")) raw

  -- TODO: Error on bad clusters
  -- TODO: This is a mess
  --let parsedClusters = map (\(fp, c) -> (fp, parseCluster (fp, c))) clusters
  let parsedClusters = clusters

  let parsedClusters' = M.fromListWith M.union (map (\(k, v) -> (takeDirectory k, M.singleton (Identifier . T.pack . takeBaseName $ k) (fromJust v))) parsedClusters)

  let parsedSpecs = rights (zipWith (curry parseSpec) specs contents)
  --putStrLn $ ppShow parsedSpecs
  -- TODO: include keys in specs, map keys to directory, merge specs + clusters
  -- into single data structure.

  defaultMain (tests parsedSpecs parsedClusters')

decodeFileWithPath path = do
    content <- Y.decodeFile path
    return (path, content)

fromRight (Right x) = x

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

  return (fromJust expr, S.fromList . map (Identifier . T.pack) . catMaybes $ spec)

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
    [ (1, Const <$> (Identifier <$> printable))
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
            , NumericRange <$> (Identifier <$> printable) <*> elements [0..10] <*> smallInt <*> smallInt
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

    traceM $ ppShow state
    step $ "Evaluating " ++ show (fromRight actual)
    expected @=? results
  where
    actual = parseRange Nothing expr
    fromRight (Right r) = r
    results = runEval state $ eval (fromRight actual)
