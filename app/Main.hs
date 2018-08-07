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
  writeFile "news-server.log" "Request : "
  writeFile "news-server.log" $ show request
  response <- catch (processRequest request) handleRequestException
  rResult <- respond response
  writeFile "news-server.log" $
    "Responded with status : " ++ (show $ responseStatus response)
  return rResult


handleRequestException :: SomeException -> IO Response
handleRequestException e = do
  writeFile "news-server.log" $ "Exception occured during request processing: " ++ show e
  return commentUpdated

commentUpdated :: Response
commentUpdated = responseLBS status500 [("Content-Type", "application/json")]
  "Error occured. Please, try again later"

main = run 3000 application