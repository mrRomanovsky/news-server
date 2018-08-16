module Main where

import Blog.Config.Config
import Blog.Routing.Routing
import Blog.Server.NewsServer
import Blog.ServerDB.DbRequests
import Control.Exception
import Database.PostgreSQL.Simple
import Network.Wai
import Network.Wai.Handler.Warp (run)

main = do
  config <- getConfig
  run (port config) $ newsServer config
