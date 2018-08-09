{-# LANGUAGE OverloadedStrings #-}

module Requests
  ( processRequest
  ) where

import Database.PostgreSQL.Simple hiding (Query)
import GetRequests
import Network.HTTP.Types (Query, methodGet, methodPost, status200)
import Network.Wai
import Data.Maybe (fromMaybe)
import System.Environment
import Network.Wai.Handler.Warp (run)
import PostRequests
import User

processRequest :: Request -> IO Response
processRequest request = do
  c <- getConnection
  let method = requestMethod request
  if method == methodGet
    then processGetRequest request c
    else processPostRequest request c

isSimpleGet :: Query -> Bool
isSimpleGet [] = True
isSimpleGet (("page", p):qs) = True
isSimpleGet (("sort_by", sB):qs) = True
isSimpleGet _ = False

getConnection :: IO Connection
getConnection = do
  dbPassword <- fromMaybe undefined <$> lookupEnv "NEWS_DB_PASSW"
  connect
    defaultConnectInfo
      { connectDatabase = "news-server"
      , connectUser = "news-server"
      , connectPassword = dbPassword
      }
