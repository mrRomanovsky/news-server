{-# LANGUAGE OverloadedStrings #-}

module Main where

import User
import Requests
import Network.Wai
import Database.PostgreSQL.Simple
import Network.HTTP.Types (status200, methodGet, methodPost, Query)
import Network.Wai.Handler.Warp (run)

--type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
{-application request respond = do
  print $ pathInfo request
  print $ queryString request
  respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello World"-}
application request respond = do
  c <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  let method = requestMethod request
      path = pathInfo request
  if method == methodGet
     then case queryString request of
      [] -> processGetRequest path c >>= respond
      qs -> processFilterGetRequest path qs c >>= respond
     else do
      rBody <- strictRequestBody request
      processPostRequest path c rBody >>= respond

                     {-query = queryString request
                     in processAppRequest path query >>= respond
            else processPostRequest request >>= respond-}

main = run 3000 application