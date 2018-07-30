{-# LANGUAGE OverloadedStrings #-}

module Main where

import User
import Requests
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

--type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application request respond = processAppRequest (pathInfo request) >>= respond

main = run 3000 application