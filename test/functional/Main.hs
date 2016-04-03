{-# LANGUAGE TemplateHaskell   #-}
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

loadRangeSpecs :: FilePath -> IO [(RangeSpec, Hrange.State)]
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
    Left err -> fail $ "Invalid spec file (" ++ path ++ "): " ++ (show err)
    Right x  -> return $ RangeSpec { _path = path, _cases = x }

-- TODO: Warn when RANGE_SPEC_PATH not set
main :: IO ()
main = do
  specPath <- lookupEnv "RANGE_SPEC_PATH"

  dirs <- maybe (return []) (\x -> listDirectories (x ++ "/spec/expand")) specPath

  specs <- mapM loadRangeSpecs dirs
  let specs' = concat specs
  defaultMain (tests specs')

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

tests specs = testGroup ""
  [ testGroup "Range Spec"              $ map (rangeSpecs id) specs
  , testGroup "Range Spec (with Cache)" $ map (rangeSpecs analyze) specs
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

rangeSpecs transform (spec, state) =
  testGroup (takeBaseName (_path spec)) $ map (specTest $ transform state) (_cases spec)

specTest state specCase =
  testCase ("Evaluating \"" ++ T.unpack expr ++ "\"") $ expected @=? actual
  where
    expr   = _query specCase
    expected = Right $ _expected specCase
    actual = expand state expr
