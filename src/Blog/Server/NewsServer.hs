module Blog.Server.NewsServer where

import Blog.Config.Config
import Blog.Config.Logging
import Blog.Exceptions.Exceptions
import Blog.Routing.Routing
import Blog.ServerDB.DbRequests
import Control.Exception (catch)
import Control.Monad (when)
import Database.PostgreSQL.Simple
import Network.Wai

newsServer :: ServerConfig -> Application
newsServer conf request respond = do
  when (logLevel conf <= Info) $ writeRequestLog request conf
  conn <- getConnection conf
  response <- catch (routers request conn) $ handleRequestException conf
  rResult <- respond response
  when (logLevel conf <= Info) $ writeResponseLog response conf
  close conn
  return rResult

writeRequestLog :: Request -> ServerConfig -> IO ()
writeRequestLog request ServerConfig {logFile = log} = do
  appendFile log "\nRequest : "
  appendFile log $ '\n' : show request

writeResponseLog :: Response -> ServerConfig -> IO ()
writeResponseLog response ServerConfig {logFile = log} =
  appendFile log $
  "\nResponded with status : " ++ show (responseStatus response)
