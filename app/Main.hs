{-# LANGUAGE OverloadedStrings #-}

module Main where

import User
import Requests
import Network.Wai
import Network.HTTP.Types (status200, methodGet, methodPost)
import Network.Wai.Handler.Warp (run)

--type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application request respond =
  let method = requestMethod request
      in if method == methodGet
            then processAppRequest (pathInfo request) >>= respond
            else processPostRequest request >>= respond

main = run 3000 application