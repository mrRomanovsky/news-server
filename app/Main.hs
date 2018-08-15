{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Blog.Exceptions.Exceptions
import Database.PostgreSQL.Simple
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Blog.Routing.Routing
import Blog.ServerDB.DbRequests

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