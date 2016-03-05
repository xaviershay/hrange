{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Types
import Parser
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Criterion.Main
import System.Environment (getArgs, withArgs)
import Control.Monad
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, mkStatus)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.ByteString.Char8 (unpack, pack)
import Data.Monoid
import Debug.Trace
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)

fromRight (Right x) = x

main :: IO ()
--main = print $ runEval $ eval state (Difference (GroupLookup (Const "hello") (Const "CLUSTER")) (Const "a"))
main = do
    args  <- getArgs
    state <- loadStateFromDirectory (args !! 0)

    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port (app state)

app :: State -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app state req respond = do
    start <- getCurrentTime
    -- CAN FAIL
    let query = fromRight $ decodeUtf8' $ fst $ head (queryString req) -- TODO: Error handle

    --let query = unEscapeString $ drop 1 $ unpack $ rawQueryString req
    ret <- respond $
        case pathInfo req of
            x -> handleQuery state query

    finish <- getCurrentTime
    let remote = show $ remoteHost req :: String
    let dt = fromRational $ toRational $ diffUTCTime finish start :: Float
    let msg = printf ("QUERY %s %.4f \"%s\"" :: String) remote dt query
    putStrLn $ printf ("%-5s [%s] %s" :: String) ("INFO" :: String) (show finish) (msg :: String)
    return ret


yay = responseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ "yay" ]

handleQuery :: State -> T.Text -> Response
handleQuery state query = do
  -- CAN FAIL
  -- NEEDS TIMEOUT
  case rangeEval state (T.unpack query) of
    Left x  -> responseBuilder (mkStatus 422 "Unprocessable Entity") [("Content-Type", "text/plain")] $ mconcat $ map copyByteString [BU.fromString $ show x]
    Right x -> success x

  where
    success results =
      responseBuilder status200 [("Content-Type", "text/plain")] $ mconcat $ map (copyByteString . BU.fromString . show) . S.toList $ results

index x = responseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p>Hello from ", BU.fromString $ show x, "!</p>"
    , "<p><a href='/yay'>yay</a></p>\n" ]
--main = do
--  args  <- getArgs
--  putStrLn "Loading state..."
--  state <- loadStateFromDirectory (args !! 0)
--  let query = args !! 1
--
--  putStrLn "Eval"
--  --case rangeEval state query of
--  --  --Left x  -> [putStrLn $ show x]
--  --  Right x -> mapM_ (putStrLn . show) . S.toList $ x
--  withArgs [] $
--    defaultMain [
--      bgroup "eval"
--        [ bench query $ whnf (S.toList . fromRight . rangeEval state) query
--        ]
--      ]
  --print $ parseRange Nothing "/a/"
  --print $ parseRange "%hello & there"
  --print $ parseRange "%hello"
  --print $ parseRange "%hello:KEYS"
  --print $ parseRange "a"
  --print $ parseRange "a,b,c"
  --print $ parseRange "a{b,c}"
  --print $ parseRange "a{b,c}d"
  --print $ parseRange "{a,b}{c,d}"
  --print $ parseRange "%{a,b}:{c,d}"
  --print $ parseRange "%hello-there"
  --print $ parseRange "hello-there"
  --print $ parseRange "hello - there"
  --print $ parseRange "a & /a/"
  --print $ parseRange "{%abc,b}"
