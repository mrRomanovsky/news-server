{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Requests (processPostRequest, processGetRequest, processFilterGetRequest) where

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

processPostRequest :: Request -> (Connection -> IO Response)
processPostRequest request c = do
  rBody <- strictRequestBody request
  let auth = lookup hAuthorization $ requestHeaders request
  case pathInfo request of
    ("users" : xs) -> postUser xs rBody auth c
    ("tags" : xs) -> authResponse auth (postTag xs rBody) c
    ("authors" : xs) -> authResponse auth (postAuthor xs rBody) c
    ("categories" : xs) -> authResponse auth (postCategory xs rBody) c
    ("drafts" : xs) -> postDraft xs rBody c

postTag :: [Text] -> B.ByteString -> Connection -> IO Response
postTag [] t = createModel $ decodeTag t
postTag ["update"] t = updateModel $ decodeTag t
postTag ["tags", "delete"] tId = deleteModel (eitherDecode tId :: Either String T.TagId)

postCategory :: [Text] -> B.ByteString -> Connection -> IO Response
postCategory [] c = createModel $ decodeCategory c
postCategory ["update"] c = updateModel $ decodeCategory c
postCategory ["delete"] cId = deleteModel (eitherDecode cId :: Either String C.CategoryId)

postUser :: [Text] -> B.ByteString -> Maybe AuthData -> Connection -> IO Response
postUser [] u _ = createModel $ decodeUser u
postUser ["delete"] uId auth = authResponse auth $
  deleteModel (eitherDecode uId :: Either String U.UserId)

postDraft :: [Text] -> B.ByteString -> Connection -> IO Response
postDraft [] d = createModel $ decodeDraft d
postDraft ["update"] d = updateModel $ decodeDraft d
postDraft ["delete"] dId = deleteModel (eitherDecode dId :: Either String D.DraftId)
postDraft ["publish"] dId = publishDraftRequest (eitherDecode dId :: Either String D.DraftId)

postAuthor :: [Text] -> B.ByteString -> Connection -> IO Response
postAuthor [] a = createModel $ decodeAuthor a
postAuthor ["update"] a = updateModel $ decodeAuthor a
postAuthor ["delete"] aId = deleteModel (eitherDecode aId :: Either String A.AuthorId)

publishDraftRequest :: Either String D.DraftId -> Connection -> IO Response
publishDraftRequest dId c = do
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

processGetRequest :: Request -> Connection -> IO Response
processGetRequest request = case pathInfo request of
  ["drafts"]     -> fmap respondJson . (read :: Connection -> IO [D.Draft])
  ["tags"]       -> fmap respondJson . (read :: Connection -> IO [T.Tag])
  ["categories"] -> fmap respondJson . (read :: Connection -> IO [C.Category])
  ["users"]      -> fmap respondJson . (read :: Connection -> IO [U.User])
  ["authors"]    ->
    let auth = lookup hAuthorization $ requestHeaders request
        in authResponse auth $ fmap respondJson . (read :: Connection -> IO [A.Author])
  ["posts"]      -> fmap respondJson . (read :: Connection -> IO [P.Post])

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

respondJson :: ToJSON m => [m] -> Response
respondJson = responseLBS status200 [("Content-Type", "application/json")] . encode

updateModel :: Model m id => Either String m -> Connection -> IO Response
updateModel model conn = do
  either (\e -> print $ "error parsing model: " ++ e) (`update` conn) model
  return $ 
    responseLBS status200 [("Content-Type", "application/json")]
      "Model was successfully updated"

deleteModel :: Model m id => Either String id -> Connection -> IO Response
deleteModel mId conn = do
  either (\e -> print $ "error parsing id: " ++ e) (`delete` conn) mId
  return $ 
    responseLBS status200 [("Content-Type", "application/json")]
      "Model was successfully deleted from the database"

createModel :: Model m id => Either String m -> Connection -> IO Response
createModel model conn = do
  either (\e -> print $ "error parsing model: " ++ e) (`create` conn) model
  return $ 
    responseLBS status200 [("Content-Type", "application/json")]
      "Model was successfully added to the database"

--notImplementedFeature = responseLBS status200 [("Content-Type", "text/plain")] "This feature is not yet implemented"