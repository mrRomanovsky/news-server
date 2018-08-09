{-# LANGUAGE OverloadedStrings #-}

module Requests
  ( processRequest
  ) where

import Database.PostgreSQL.Simple hiding (Query)
import GetRequests
import Network.HTTP.Types (Query, methodGet, methodPost, status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import PostRequests
import User

processRequest :: Request -> IO Response
processRequest request = do
  c <-
    connect
      defaultConnectInfo
        { connectDatabase = "news-server"
        , connectUser = "news-server"
        , connectPassword = "news-server"
        }
  let method = requestMethod request
  if method == methodGet
    then processGetRequest request c
    else processPostRequest request c

isSimpleGet :: Query -> Bool
isSimpleGet [] = True
isSimpleGet (("page", p):qs) = True
isSimpleGet (("sort_by", sB):qs) = True
isSimpleGet _ = False
