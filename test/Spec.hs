{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import           Hrange
import           Lib (runEval, eval)
import           Parser
import           Types

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
import System.Directory
import System.FilePath.Posix (joinPath)
import System.IO
import Data.List

data RangeSpecCase = RangeSpecCase {
  _query :: String,
  _expected :: Result
} deriving (Show)

data RangeSpec = RangeSpec {
  _path  :: FilePath,
  _cases :: [RangeSpecCase]
} deriving (Show)

listDirectories :: FilePath -> IO [FilePath]
listDirectories path = do
  e <- doesDirectoryExist path
  if e
    then do
      files <- getDirectoryContents path
      let dirs     = filter (not . isPrefixOf ".") files
      let fullDirs = map (\x -> joinPath [path, x]) dirs
      ds' <- mapM listDirectories fullDirs

      return (path:concat ds')
    else return []

loadRangeSpecs :: FilePath -> IO [(RangeSpec, Types.State)]
loadRangeSpecs dir = do
  (state, _) <- loadStateFromDirectory dir
  specs <- getDirectoryContents dir
  let specs' = map (\x -> joinPath [dir, x]) $ filter (isSuffixOf ".spec") specs

  loadedSpecs <- mapM (\x -> withFile x ReadMode (doParse x)) specs'

  return $ map (\x -> (x, analyze state)) loadedSpecs

doParse :: String -> Handle -> IO RangeSpec
doParse path handle = do
  contents <- hGetContents handle

  case parse rangeSpec path contents of
    Left err    -> fail $ "Invalid spec file: " ++ path
    Right parse -> return $ RangeSpec { _path = path, _cases = parse }

parseSpec :: (String, String) -> Either ParseError (String, [RangeSpecCase])
parseSpec (name, input) = f p
  where
    p = parse rangeSpec name input
    f (Right parse) = Right (name, parse)
    f (Left err)    = Left err

-- TODO: Error on spec parse failure
-- TODO: Warn when RANGE_SPEC_PATH not set
main :: IO ()
main = do
  specPath <- lookupEnv "RANGE_SPEC_PATH"

  dirs <- maybe (return []) (\x -> listDirectories (x ++ "/spec/expand")) specPath

  specs <- mapM loadRangeSpecs dirs
  let specs' = concat specs
  defaultMain (tests specs')

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

  return $ RangeSpecCase (fromJust expr) (S.fromList . map T.pack . catMaybes $ spec)

rangeSpec = many rangeSingleSpec

tests specs = testGroup ""
  [ testGroup "Range Spec"              $ map (rangeSpecs id) specs
  , testGroup "Range Spec (with Cache)" $ map (rangeSpecs analyze) specs
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
            , FunctionMem   <$> arbitrary <*> arbitrary
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
  [ testProperty "eval is fully defined" $
      \expr -> runEval emptyState (eval expr) `seq` True
  ]

rangeSpecs transform (spec, state) =
  testGroup (takeBaseName (_path spec)) $ map (specTest $ transform state) (_cases spec)

specTest state specCase =
  testCaseSteps ("Evaluating \"" ++ expr ++ "\"") $ \step -> do
    step "Parsing"
    assert $ isRight actual

    step $ "Evaluating " ++ show (fromRight actual)
    expected @=? results
  where
    expr   = _query specCase
    expected = _expected specCase
    actual = parseRange Nothing expr
    fromRight (Right r) = r
    results = runEval state $ eval (fromRight actual)
