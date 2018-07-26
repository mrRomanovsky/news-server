{-# LANGUAGE OverloadedStrings #-}

module Main where

import User
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

--type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application request respond = do
  print $ pathInfo request --array of path parts, divided with slashes, left to right
  respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

main = run 3000 application