{-# LANGUAGE OverloadedStrings #-}

module Main where

import User
import Requests
import Network.Wai
import Database.PostgreSQL.Simple
import Network.HTTP.Types (status200, methodGet, methodPost, Query)
import Network.Wai.Handler.Warp (run)

--type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application :: Application
application request respond = processRequest request >>= respond

main = run 3000 application