{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple hiding (Query)
import Network.HTTP.Types (status500)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Routing
import System.Environment

main = run 3000 application

application :: Application
application request respond = do
  appendFile "news-server.log" "\nRequest : "
  appendFile "news-server.log" $ '\n' : show request
  c <- getConnection
  response <- catch (routers request c) handleRequestException
  rResult <- respond response
  appendFile "news-server.log" $
    "\nResponded with status : " ++ show (responseStatus response)
  close c
  return rResult

getConnection :: IO Connection
getConnection = do
  dbPassword <- fromMaybe undefined <$> lookupEnv "NEWS_DB_PASSW"
  connect
    defaultConnectInfo
      { connectDatabase = "news-server"
      , connectUser = "news-server"
      , connectPassword = dbPassword
      }

handleRequestException :: SomeException -> IO Response
handleRequestException e = do
  appendFile "news-server.log" $
    "\nException occured during request processing: " ++ show e
  return errorOccured

errorOccured :: Response
errorOccured =
  responseLBS
    status500
    [("Content-Type", "application/json")]
    "Error occured. Please, try again later"
