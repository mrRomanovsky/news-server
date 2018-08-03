{-# LANGUAGE OverloadedStrings #-}

module Main where

import User
import Requests
import Network.Wai
import Network.HTTP.Types (status200, methodGet, methodPost, Query)
import Network.Wai.Handler.Warp (run)

--type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
{-application request respond = do
  print $ pathInfo request
  print $ queryString request
  respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello World"-}
application request respond =
  let method = requestMethod request
      in if method == methodGet
            then let path = pathInfo request
                     query = queryString request
                     in processAppRequest path query >>= respond
            else processPostRequest request >>= respond

main = run 3000 application