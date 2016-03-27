module Main where

import           Hrange

import qualified Data.HashSet       as S
import           Data.List          (sort)
import qualified Data.Text          as T
import           System.Environment (getArgs)
import           System.Exit        (die)

main :: IO ()
main = do
    args  <- getArgs

    case length args of
      2 -> return ()
      _ -> die "Usage: ergh DIR QUERY"

    let stateDir = args !! 0
    let query = args !! 1

    (state, _) <- loadStateFromDirectory stateDir

    case expand state query of
      Left err      -> die $ "Invalid query: " ++ show err
      Right results -> putStr . T.unpack . T.unlines . sort . S.toList $ results
