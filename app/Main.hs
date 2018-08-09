{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Database.PostgreSQL.Simple
import Network.HTTP.Types (Query, methodGet, methodPost, status200, status500)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Requests
import User

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
  appendFile "news-server.log" $
    "\nException occured during request processing: " ++ show e
  return commentUpdated

commentUpdated :: Response
commentUpdated =
  responseLBS
    status500
    [("Content-Type", "application/json")]
    "Error occured. Please, try again later"

main = run 3000 application
