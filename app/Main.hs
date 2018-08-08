{-# LANGUAGE OverloadedStrings #-}

module Main where

import User
import Requests
import Control.Exception
import Network.Wai
import Database.PostgreSQL.Simple
import Network.HTTP.Types (status200, status500, methodGet, methodPost, Query)
import Network.Wai.Handler.Warp (run)

--type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application :: Application
application request respond = do
  appendFile "news-server.log" "\nRequest : "
  appendFile "news-server.log" $ '\n' : show request
  response <- catch (processRequest request) handleRequestException
  rResult <- respond response
  appendFile "news-server.log" $
    "\nResponded with status : " ++ show (responseStatus response)
  return rResult

handleRequestException :: SomeException -> IO Response
handleRequestException e = do
  appendFile "news-server.log" $ "\nException occured during request processing: " ++ show e
  return commentUpdated

commentUpdated :: Response
commentUpdated = responseLBS status500 [("Content-Type", "application/json")]
  "Error occured. Please, try again later"

main = run 3000 application