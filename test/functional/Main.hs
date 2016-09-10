{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Signatures aren't that useful in this file.
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import           Hrange

import qualified Data.HashSet          as S
import           Data.List
import           Data.Maybe
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import           System.Directory
import           System.Environment    (lookupEnv)
import           System.FilePath       (takeBaseName)
import           System.FilePath.Posix (joinPath)
import           System.IO
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec

data RangeSpecCase = RangeSpecCase {
  _query :: T.Text,
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

loadSpecs :: FilePath -> IO [(RangeSpec, Hrange.State)]
loadSpecs dir = do
  (state, invalid) <- loadStateFromDirectory dir

  case invalid of
    [] -> do
      specs <- getDirectoryContents dir
      let specs' = map (\x -> joinPath [dir, x]) $ filter (isSuffixOf ".spec") specs

      loadedSpecs <- mapM (\x -> withFile x ReadMode (doParse x)) specs'

      return $ map (\x -> (x, analyze state)) loadedSpecs
    _  -> fail $ "Could not parse YAML in range spec: " <> show invalid

doParse :: String -> Handle -> IO RangeSpec
doParse path handle = do
  contents <- hGetContents handle

  case parse rangeSpec path contents of
    Left err -> fail $ "Invalid spec file (" ++ path ++ "): " ++ show err
    Right x  -> return RangeSpec { _path = path, _cases = x }

main :: IO ()
main = do
  specPath' <- lookupEnv "RANGE_SPEC_PATH"
  let specPath = maybe "range-spec" id specPath'

  expandDirs   <- listDirectories (specPath <> "/spec/expand")
  compressDirs <- listDirectories (specPath <> "/spec/compress")

  expandSpecs   <- mapM loadSpecs expandDirs
  compressSpecs <- mapM loadSpecs compressDirs

  let expandSpecs'   = concat expandSpecs
  let compressSpecs' = concat compressSpecs

  defaultMain (tests expandSpecs' compressSpecs')

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

  return $ RangeSpecCase (T.pack . fromJust $ expr) (S.fromList . map T.pack . catMaybes $ spec)

rangeSpec = many rangeSingleSpec

tests expandSpecs compressSpecs = testGroup ""
  [ testGroup "Range Spec (expand)"            $ map (mkExpandTestGroup id) expandSpecs
  , testGroup "Range Spec (expand with Cache)" $ map (mkExpandTestGroup analyze) expandSpecs
  , testGroup "Range Spec (validate compress)" $ map (mkExpandTestGroup id) compressSpecs
  , testGroup "Range Spec (compress)" $ map mkCompressTestGroup compressSpecs
  , testGroup "Parsing errors" parseErrorSpecs
  ]

parseErrorSpecs =
  [ testParseError "has()" "expecting 2 arguments, got 0"
  , testParseError "has(" "expecting expression or closing )"
  , testParseError "has(a;)" "unexpected \")\""
  , testParseError "(" "expecting expression or closing )"
  ]
  where
    testParseError query expected = testCase (query <> " / " <> expected) $
      case expand emptyState (T.pack query) of
        Left err -> expected `isInfixOf` show err @?
                      ("Expected error to include: " <> expected <> "\nError was:\n" <> show err)
        Right _  -> assertFailure "Was a valid parse"

mkExpandTestGroup transform (spec, state) =
  testGroup (takeBaseName (_path spec)) $ map (expandTest $ transform state) (_cases spec)

mkCompressTestGroup (spec, _) =
  testGroup (takeBaseName (_path spec)) $ map compressTest (_cases spec)

expandTest state specCase =
  testCase ("Evaluating \"" ++ T.unpack expr ++ "\"") $ expected @=? actual
  where
    expr   = _query specCase
    expected = Right $ _expected specCase
    actual = expand state expr

compressTest specCase =
  testCase ("Compressing \"" ++ T.unpack expr ++ "\"") $ expected @=? actual
  where
    expr     = _query specCase
    expected = expr
    actual   = compress $ _expected specCase

