module Main where

import Blog.Config.Config
import Blog.Server.NewsServer
import Network.Wai.Handler.Warp (run)

main = do
  config <- getConfig
  run (port config) $ newsServer config
