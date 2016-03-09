{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import           Lib
import           Parser
import           Types
import           Yaml

import           Control.Monad          (replicateM)
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Either
import qualified Data.HashMap.Strict    as M
import qualified Data.HashSet           as S
import           Data.Maybe
import           Data.Scientific        (Scientific, isInteger,
                                         toBoundedInteger)
import qualified Data.Text              as T
import qualified Data.Vector            as V
import qualified Data.Yaml              as Y
import           System.Environment     (lookupEnv)
import           System.FilePath        (takeBaseName, takeDirectory)
import           System.FilePath.Find
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.Parsec
import qualified Text.Regex.TDFA        as R
import           Text.Show.Pretty


type RangeSpec = M.HashMap String [S.HashSet String]

-- TODO: Error on spec parse failure
-- TODO: Warn when RANGE_SPEC_PATH not set
main :: IO ()
main = do
  specPath <- lookupEnv "RANGE_SPEC_PATH"
  specs <- maybe (return []) (\x -> find always (extension ==? ".spec") (x ++ "/spec/expand")) specPath
  yamls <- maybe (return []) (\x -> find always (extension ==? ".yaml") (x ++ "/spec/expand")) specPath

  contents <- mapM readFile specs
  raw      <- mapM decodeFileWithPath yamls

  let clusters = map parseClusters raw

  -- TODO: Error on bad clusters
  let parsedClusters = rights clusters

  -- TODO: This is a mess
  let parsedClusters' = M.fromListWith M.union (map (\(k, v) -> (takeDirectory k, M.singleton (T.pack . takeBaseName $ k) v)) parsedClusters)

  let parsedSpecs = rights (zipWith (curry parseSpec) specs contents)

  defaultMain (tests parsedSpecs parsedClusters')

  where
    parseClusters (path, Nothing) = fail "Invalid YAML"
    parseClusters (path, Just x) = do
      cluster <- runReader (runExceptT $ parseYAML x) (takeBaseName path)

      return (path, cluster)

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
  [ testProperty "eval is fully defined" $
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
