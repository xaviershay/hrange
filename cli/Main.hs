module Main where

import           Hrange

import           Data.Foldable      (toList)
import           Data.List          (sort)
import qualified Data.Text          as T
import           System.Environment (getArgs)
import           System.Exit        (die)
import           Text.Show.Pretty

main :: IO ()
main = do
    args  <- getArgs

    case length args of
      2 -> return ()
      _ -> die "Usage: ergh DIR QUERY"

    let stateDir = args !! 0
    let query = args !! 1

    (state, _) <- loadStateFromDirectory stateDir
    let state' = analyze state

    putStrLn $ ppShow state'
    putStrLn ""

    case expandDebug state' query of
      Left err             -> die $ "Invalid query: " ++ show err
      Right (results, debug) -> do
                                  putStrLn $ ppShow debug
                                  putStrLn ""
                                  putStr . T.unpack . T.unlines . sort . toList $ results
