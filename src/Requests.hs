{-# LANGUAGE OverloadedStrings #-}
module Requests (processRequest) where

import PostRequests
import GetRequests
import User
import Network.Wai
import Database.PostgreSQL.Simple hiding (Query)
import Network.HTTP.Types (status200, methodGet, methodPost, Query)
import Network.Wai.Handler.Warp (run)

processRequest :: Request -> IO Response
processRequest request = do
  c <- connect defaultConnectInfo {
      connectDatabase = "news-server"
    , connectUser = "news-server"
    , connectPassword = "news-server" 
  }
  let method = requestMethod request
      qString = queryString request
  if method == methodGet
     then if isSimpleGet qString
             then processGetRequest request c
             else processFilterGetRequest request c
     else
      processPostRequest request c

isSimpleGet :: Query -> Bool
isSimpleGet [] = True
isSimpleGet (("page", p) : qs) = True
isSimpleGet (("sort_by", sB) : qs) = True
isSimpleGet _ = False