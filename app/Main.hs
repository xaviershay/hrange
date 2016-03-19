{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Types
import Parser
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8Builder)
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
import Text.Show.Pretty (ppShow)
import Control.Exception (evaluate)
import Control.DeepSeq (deepseq, ($!!))
import Data.Maybe (fromJust)

fromRight (Right x) = x

main :: IO ()
--main = print $ runEval $ eval state (Difference (GroupLookup (Const "hello") (Const "CLUSTER")) (Const "a"))
main = do
    args  <- getArgs
    putStrLn "Loading state"
    start <- getCurrentTime
    state <- loadStateFromDirectory (args !! 0)
    finish <- getCurrentTime
    putStrLn "Loaded state"

    let port = 3000
    let state' = analyze state
    state' `deepseq` do
      putStrLn $ "Listening on port " ++ show port
      run port (app state')

-- TODO: Support timeouts
withTiming :: a -> IO (Float, a)
withTiming action = do
  start  <- getCurrentTime
  result <- evaluate action
  finish <- getCurrentTime

  let dt = fromRational $ toRational $ diffUTCTime finish start :: Float

  return (dt, result)

buildResponse :: State -> Request -> (Response, Maybe T.Text)
buildResponse state req =
    let (status, extra, content) = case handleQuery2 state req of
                                     Left err -> (mkStatus 422 "Unprocessable Entity", Nothing, err)
                                     Right (query, results) -> (status200, Just query, results) in

    -- Use evaluate at seq to force evaluation so that timing is accurate
    let resp = content `seq` responseBuilder status [("Content-Type", "text/plain")] $ encodeUtf8Builder content in

    (resp, extra)

app :: State -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app state req respond = do
    (dt, (resp, extra)) <- withTiming (buildResponse state req)

    -- TODO: Extract logging elsewhere
    let remote = show $ remoteHost req :: String
    let msg = printf ("%s %.4f /%s \"%s\"" :: String) remote dt (T.unpack $ T.intercalate "/" $ pathInfo req) (T.replace "\"" "\\\"" (maybe "" id extra))
    logInfo msg
    respond resp

logInfo :: String -> IO ()
logInfo msg = do
    currentTime <- getCurrentTime
    let level = "INFO" :: String
    putStrLn $ printf ("%-5s [%s] %s" :: String) level (show currentTime) msg

decodeQuery :: Request -> Either T.Text T.Text
decodeQuery req = do
  query <- f (queryString req)
  either (Left . T.pack . show) Right (decodeUtf8' $ fst $ query)

  where
    f (x:_) = Right x
    f _     = Left "No query present"

handleQuery2 :: State -> Request -> Either T.Text (T.Text, T.Text)
handleQuery2 state req = do
    query  <- decodeQuery req
    result <- either (Left . T.pack . show) Right (rangeEval state (T.unpack query))

    return (query, T.unlines . S.toList $ result)

--(Status, LogExtra, Text)
-- SUCCESS: 200 (or Text)
-- Invalid unicode: 422 Text error
-- Invalid query: 422 Text error
handleQuery :: State -> T.Text -> IO Response
handleQuery state query = do
  -- CAN FAIL
  return $ case rangeEval state (T.unpack query) of
    Left x  -> responseBuilder (mkStatus 422 "Unprocessable Entity") [("Content-Type", "text/plain")] $ mconcat $ map copyByteString [BU.fromString $ show x]
    Right x -> success x

  where
    success results =
      responseBuilder status200 [("Content-Type", "text/plain")] $ mconcat $ map (copyByteString . BU.fromString . show) . S.toList $ results

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
