{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Hrange
import           Logging

import           Control.Exception        (evaluate)
import           Data.Foldable            (toList)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8', encodeUtf8Builder)
import           Network.HTTP.Types       (Status, mkStatus, status200)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Environment       (getArgs)
import           System.Microtimer        (time)

main :: IO ()
main = do
    args  <- getArgs
    logInfo $ fromText "Loading state"

    (dt, (state, _)) <- time $ loadStateFromDirectory (args !! 0)
    logInfo $ build "Loaded state in {}, Analyzing..." (Only $ fixed 4 dt)

    let port = 3000

    (dt', analyzedState) <- time $ evaluate (analyze' state)

    logInfo $ build "Analyzed state in {}" (Only $ fixed 4 dt')
    logInfo $ build "Listening on port {}" (Only port)
    run port (app analyzedState)

app :: State -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app state req respond = do
    let query = decodeQuery req

    case query of
      Left err ->
        respond $ textResponse status422 err
      Right x  -> do
        (dt, results) <- time . evaluate $ expand' state x

        case results of
          Left err ->
            respond $ textResponse status422 (T.pack . show $ err)
          Right y -> do
            logInfoReq req (build "{} \"{}\"" (fixed 4 dt, x))
            respond $ textResponse status200 (T.unlines . toList $ y)

textResponse :: Status -> T.Text -> Response
textResponse status body =
  responseBuilder status [("Content-Type", "text/plain")] (encodeUtf8Builder body)

status422 :: Network.HTTP.Types.Status
status422 = mkStatus 422 "Unprocessable Entity"

decodeQuery :: Request -> Either T.Text T.Text
decodeQuery req = do
  query <- f (queryString req)
  either (Left . T.pack . show) Right (decodeUtf8' $ fst $ query)

  where
    f (x:_) = Right x
    f _     = Left "No query present"
