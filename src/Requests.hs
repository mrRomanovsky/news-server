{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Requests ( processPostRequest, processGetRequest, processFilterGetRequest
                , authorizeAdmin) where

import Prelude hiding (read)
import qualified Tag as T
import qualified Post as P
import qualified Draft as D
import qualified User as U
import qualified Author as A
import qualified Category as C
import Model
import DbRequests
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson
import Data.Attoparsec.Text
import Network.Wai
import Network.HTTP.Types (status200, hAuthorization, Query)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Database.PostgreSQL.Simple.Time
import Database.PostgreSQL.Simple hiding (Query)
import Database.PostgreSQL.Simple.Types hiding (Query)
import Database.PostgreSQL.Simple.FromRow

processPostRequest :: [Text] -> Connection -> B.ByteString -> IO Response
processPostRequest ["users"] c = createModel c . decodeUser
processPostRequest ["users", "delete"] c =
  deleteModel c . (eitherDecode :: B.ByteString -> Either String U.UserId)
processPostRequest ["authors"] c = createModel c . decodeAuthor
processPostRequest ["authors", "update"] c = updateModel c . decodeAuthor
processPostRequest ["authors", "delete"] c =
  deleteModel c . (eitherDecode :: B.ByteString -> Either String A.AuthorId)
processPostRequest ["tags"] c = createModel c . decodeTag
processPostRequest ["tags", "update"] c = updateModel c . decodeTag
processPostRequest ["tags", "delete"] c =
  deleteModel c . (eitherDecode :: B.ByteString -> Either String T.TagId)
processPostRequest ["categories"] c = createModel c . decodeCategory
processPostRequest ["categories", "update"] c = updateModel c . decodeCategory
processPostRequest ["categories", "delete"] c =
  deleteModel c . (eitherDecode :: B.ByteString -> Either String C.CategoryId)
processPostRequest ["drafts"] c =  createModel c . decodeDraft
processPostRequest ["drafts", "update"] c =  updateModel c . decodeDraft
processPostRequest ["drafts", "delete"] c =
  deleteModel c . (eitherDecode :: B.ByteString -> Either String D.DraftId)
processPostRequest ["drafts", "publish"] c = 
  publishDraftRequest c . eitherDecode

publishDraftRequest :: Connection -> Either String D.DraftId -> IO Response
publishDraftRequest c dId = do
  either (\e -> print $ "error parsing draft id: " ++ e) (`D.publishDraft` c) dId
  return $ 
    responseLBS status200 [("Content-Type", "application/json")]
      "Draft was successfully published"

--return 40X in other cases
decodeUser = eitherDecode :: B.ByteString -> Either String U.User
decodeAuthor = eitherDecode :: B.ByteString -> Either String A.Author
decodeTag = eitherDecode :: B.ByteString -> Either String T.Tag
decodeCategory = eitherDecode :: B.ByteString -> Either String C.Category
decodeDraft = eitherDecode :: B.ByteString -> Either String D.Draft

processGetRequest :: [Text] -> Connection -> IO Response
processGetRequest ["drafts"] c = respondJson <$> (read c :: IO [D.Draft])
processGetRequest ["tags"] c = respondJson <$> (read c :: IO [T.Tag])
processGetRequest ["categories"] c = respondJson <$> (read c :: IO [C.Category])
processGetRequest ["users"] c = respondJson <$> (read c :: IO [U.User])
processGetRequest ["authors"] c = respondJson <$> (read c :: IO [A.Author])
processGetRequest ["posts"] c = respondJson <$> (read c :: IO [P.Post])

processFilterGetRequest :: [Text] -> Query -> Connection -> IO Response
processFilterGetRequest ["posts"] [("author_name", Just author)] c =
  respondJson <$> P.getPostsByAuthor author c
processFilterGetRequest ["posts"] [("content_substr", Just author)] c =
  respondJson <$> P.getPostsWithSubstrInContent author c
processFilterGetRequest ["posts"] [("name_substr", Just author)] c =
  respondJson <$> P.getPostsWithSubstrInName author c
processFilterGetRequest ["posts"] [("tag", Just tag)] c =
  respondJson <$> P.getPostsWithTag tag c
processFilterGetRequest ["posts"] [("tags_in", Just tagsIn)] c =
  respondJson <$> P.getPostsTagsIn tagsIn c
processFilterGetRequest ["posts"] [("tags_all", Just tagsAll)] c =
  respondJson <$> P.getPostsTagsAll tagsAll c
processFilterGetRequest ["posts"] [("created_at", Just date)] c =
  respondJson <$> P.getPostsDate date c
processFilterGetRequest ["posts"] [("created_at__lt", Just dateLt)] c =
  respondJson <$> P.getPostsDateLt dateLt c
processFilterGetRequest ["posts"] [("created_at__gt", Just dateGt)] c =
  respondJson <$> P.getPostsDateGt dateGt c
{-
/posts?created_at=2018-05-21
/posts?created_at__lt=2018-05-21
/posts?created_at__gt=2018-05-21
-}
authorizeAdmin :: Request -> Connection -> IO Bool
authorizeAdmin request conn = 
  let headers = requestHeaders request
      auth = lookup hAuthorization headers
      in maybe (return False) (`checkAdmin` conn) auth

respondJson :: ToJSON m => [m] -> Response
respondJson = responseLBS status200 [("Content-Type", "application/json")] . encode

updateModel :: Model m id => Connection -> Either String m -> IO Response
updateModel conn model = do
  either (\e -> print $ "error parsing model: " ++ e) (`update` conn) model
  return $ 
    responseLBS status200 [("Content-Type", "application/json")]
      "Model was successfully updated"

deleteModel :: Model m id => Connection -> Either String id -> IO Response
deleteModel conn mId = do
  either (\e -> print $ "error parsing id: " ++ e) (`delete` conn) mId
  return $ 
    responseLBS status200 [("Content-Type", "application/json")]
      "Model was successfully deleted from the database"

createModel :: Model m id => Connection -> Either String m -> IO Response
createModel conn model = do
  either (\e -> print $ "error parsing model: " ++ e) (`create` conn) model
  return $ 
    responseLBS status200 [("Content-Type", "application/json")]
      "Model was successfully added to the database"

--notImplementedFeature = responseLBS status200 [("Content-Type", "text/plain")] "This feature is not yet implemented"