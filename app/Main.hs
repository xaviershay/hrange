{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Hrange
import           Logging
import           State

import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Exception        (evaluate)
import           Control.Lens             ((^.))
import           Control.Monad            (forever)
import           Data.Foldable            (toList)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8', decodeUtf8With,
                                           encodeUtf8Builder)
import           Data.Text.Encoding.Error (lenientDecode)
import           Network.HTTP.Types       (Status, mkStatus, status200,
                                           statusCode)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Environment       (getArgs)
import           System.Exit              (die)
import           System.Microtimer        (time)
import           System.Timeout           (timeout)

main :: IO ()
main = do
    args  <- getArgs

    stateDir <- case args of
                  [x] -> return x
                  _   -> die "Usage: hrange-server path/to/state"

    _ <- spawnLogThread
    stateContainer <- atomically (newTVar newStateContainer)

    reloadState stateDir stateContainer

    _ <- forkIO . forever $ do
           threadDelay defaultReload
           reloadState stateDir stateContainer

    let port = 3000
    logInfo $ build "Listening on port {}" (Only port)

    run port (app stateContainer)

reloadState :: FilePath -> TVar StateContainer -> IO ()
reloadState stateDir stateContainer = do
    logInfo $ fromText "Loading state"
    (dt, (state, _)) <- time $ loadStateFromDirectory stateDir
    logInfo $ build "Loaded state in {}, Analyzing..." (Only $ fixed 4 dt)
    (dt', analyzedState) <- time $ evaluate (analyze' state)
    logInfo $ build "Analyzed state in {}" (Only $ fixed 4 dt')

    atomically (modifyTVar stateContainer (updateStateContainer "arst" analyzedState))

app :: TVar StateContainer -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app stateContainer req respond = do
  existing <- atomically (readTVar stateContainer)

  let state = existing ^. mostRecent

  logInfo $ fromText (toIdentifier state)

  (dt, response) <- withTimeoutResponse defaultTimeout $
                    withValidQuery req $
                    rangeResponse state

  let rawQuery = T.drop 1 . decodeUtf8With lenientDecode $ rawQueryString req

  logInfoReq req $ build "{} {} \"{}\""
    (fixed 4 dt, responseCode response, rawQuery)

  respond response

defaultTimeout :: Int
defaultTimeout = 300000

defaultReload :: Int
defaultReload = 60 * 1000000 -- One minute

withTimeoutResponse :: Int -> Response -> IO (Double, Response)
withTimeoutResponse t f = do
  (dt, maybeResponse) <- time . timeout t . evaluate $ f

  let timeoutResponse = textResponse status422 "Query exceeded time limit"
  let response = fromMaybe timeoutResponse maybeResponse

  return (dt, response)

withValidQuery :: Request -> (T.Text -> Response) -> Response
withValidQuery req f =
  either
    (textResponse status422)
    f
    (decodeQuery req)

rangeResponse :: State -> Query -> Response
rangeResponse state q =
  either
    (textResponse status422 . T.pack . show)
    (textResponse status200 . T.unlines . toList)
    (expand' state q)

responseCode :: Response -> Int
responseCode = statusCode . responseStatus

textResponse :: Status -> T.Text -> Response
textResponse status body =
  responseBuilder status [("Content-Type", "text/plain")] (encodeUtf8Builder body)

status422 :: Network.HTTP.Types.Status
status422 = mkStatus 422 "Unprocessable Entity"

decodeQuery :: Request -> Either T.Text T.Text
decodeQuery req = do
  query <- f (queryString req)
  either (Left . T.pack . show) Right (decodeUtf8' . fst $ query)

  where
    f (x:_) = Right x
    f _     = Left "No query present"
