{-# LANGUAGE OverloadedStrings #-}

module CommentRequests
  ( getComments
  , createComment
  , CommentRequests.deleteComment
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text
import Database.PostgreSQL.Simple
import DbRequests
import Model
import Network.HTTP.Types (hAuthorization, status200)
import Network.Wai
import PostDTO as P
import Prelude hiding (read)
import RequestsUtils

getComments :: Request -> Connection -> IO Response
getComments request =
  let postNumber = getPostNumber request
      (page, _, _) = getAdditionalParams request
   in fmap respondJson . P.getPostComments postNumber page

createComment :: Request -> Connection -> IO Response
createComment request c = do
  let postNumber = getPostNumber request
  comment <- strictRequestBody request
  P.insertComment postNumber comment c
  commentUpdated

deleteComment :: Request -> Connection -> IO Response
deleteComment request c = do
  let postNumber = getPostNumber request
  commentId <- strictRequestBody request
  P.deleteComment postNumber commentId c
  commentDeleted

commentUpdated :: IO Response
commentUpdated =
  return $
  responseLBS
    status200
    [("Content-Type", "application/json")]
    "Comment was successfully added"

commentDeleted :: IO Response
commentDeleted =
  return $
  responseLBS
    status200
    [("Content-Type", "application/json")]
    "Comment was successfully deleted"

getPostNumber :: Request -> Text
getPostNumber request =
  case pathInfo request of
    (x:postNumber:xs) -> postNumber
    _ -> error "comments request should contain post number!"
