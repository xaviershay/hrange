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

import Lib
import Parser

import System.Environment (lookupEnv)

type RangeSpec = M.Map String [S.Set String]

main :: IO ()
main = do
  specPath <- lookupEnv "RANGE_SPEC_PATH"
  specs <- maybe (return []) (\x -> find always (extension ==? ".spec") (x ++ "/spec/expand")) specPath

  contents <- mapM readFile specs

  defaultMain (tests $ rights (map parseSpec (zip specs contents)))

  where

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

  return $ (fromJust expr, S.fromList . catMaybes $ spec)

rangeSpec = do
  specs <- many rangeSingleSpec

  return $ specs

parseSpec :: (String, String) -> Either ParseError [(String, S.Set String)]
parseSpec (name, input) = parse rangeSpec name input

tests specs = testGroup "Range Spec" $ map rangeSpecs specs

rangeSpecs specs = testGroup "" $ map specTest specs

specTest (expr, expected) = testCase ("Parsing \"" ++ expr ++ "\"") $ assert $ isRight actual
  where
    actual = parseRange expr
