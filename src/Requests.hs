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
import Data.Text hiding (head)
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson
import Data.Attoparsec.Text
import Network.Wai
import Network.HTTP.Types (status200, status422, hAuthorization, Query)
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
    ("drafts" : xs) -> postDraft xs rBody auth c

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

postDraft :: [Text] -> B.ByteString -> Maybe AuthData -> Connection -> IO Response
postDraft path d auth c = do
  aId <- maybe (return Nothing) (`getAuthorId` c) auth 
  maybe (return notFound) (\a -> postDraftUser path d a c) aId

postDraftUser :: [Text] -> B.ByteString -> Integer -> Connection -> IO Response
postDraftUser [] d aId = createModel $ draftWithAuthor d aId
postDraftUser ["update"] d aId = updateModel $ draftWithAuthor d aId
postDraftUser ["delete"] dId aId = draftIdAction dId aId deleteModel
postDraftUser ["publish"] dId aId = draftIdAction dId aId publishDraftRequest

draftWithAuthor :: B.ByteString -> Integer -> Either String D.Draft
draftWithAuthor d aId = setDraftAuthor aId <$> decodeDraft d

draftIdAction :: B.ByteString -> Integer -> DraftIdAction -> Connection -> IO Response
draftIdAction dId aId action c = do
  let draftId = eitherDecode dId :: Either String D.DraftId
  either (const $ return notFound) (\(D.DraftId drId) -> do
    draftAuthor <- getDraftAuthor drId c
    if draftAuthor == aId
      then action draftId c
      else return notFound) draftId

setDraftAuthor :: Integer -> D.Draft -> D.Draft
setDraftAuthor aId d = d{D.authorId = aId}

postAuthor :: [Text] -> B.ByteString -> Connection -> IO Response
postAuthor [] a = createModel $ decodeAuthor a
postAuthor ["update"] a = updateModel $ decodeAuthor a
postAuthor ["delete"] aId = deleteModel (eitherDecode aId :: Either String A.AuthorId)

type DraftIdAction = Either String D.DraftId -> Connection -> IO Response

publishDraftRequest :: Either String D.DraftId -> Connection -> IO Response
publishDraftRequest dId c = do
  either (\e -> print $ "error parsing draft id: " ++ e) (`D.publishDraft` c) dId
  return $ 
    responseLBS status200 [("Content-Type", "application/json")]
      "Draft was successfully published"

--deleteModel :: Model m id => Either String id -> Connection -> IO Response

--return 40X in other cases
decodeUser = eitherDecode :: B.ByteString -> Either String U.User
decodeAuthor = eitherDecode :: B.ByteString -> Either String A.Author
decodeTag = eitherDecode :: B.ByteString -> Either String T.Tag
decodeCategory = eitherDecode :: B.ByteString -> Either String C.Category
decodeDraft = eitherDecode :: B.ByteString -> Either String D.Draft

processGetRequest :: Request -> Connection -> IO Response
processGetRequest request =
  let mPage = lookup "page" $ queryString request
      page = mPage >>=
        maybe Nothing (decode . B.fromStrict:: BS.ByteString -> Maybe Integer)
      in case pathInfo request of
  ["drafts"]     -> fmap respondJson . (read page :: Connection -> IO [D.Draft])
  ["tags"]       -> fmap respondJson . (read page :: Connection -> IO [T.Tag])
  ["categories"] -> fmap respondJson . (read page :: Connection -> IO [C.Category])
  ["users"]      -> fmap respondJson . (read page :: Connection -> IO [U.User])
  ["authors"]    ->
    let auth = lookup hAuthorization $ requestHeaders request
        in authResponse auth $ fmap respondJson . (read page :: Connection -> IO [A.Author])
  ["posts"]      -> fmap respondJson . (read page :: Connection -> IO [P.Post])

{-processPostsGetRequest :: Query -> Connection -> IO Response
processPostsGetRequest-}

--processFilterGetRequest :: [Text] -> Query -> Connection -> IO Response
processFilterGetRequest :: Request -> Connection -> IO Response
processFilterGetRequest request c = 
  let path = pathInfo request --add path processing!
      queryStr = queryString request
      mPage = lookup "page" queryStr
      page = mPage >>=
        maybe Nothing (decode . B.fromStrict:: BS.ByteString -> Maybe Integer)
      in case head queryStr of
        ("author_name", Just author) ->
          respondJson <$> P.getPostsByAuthor author page c
        ("content_substr", Just author) ->
          respondJson <$> P.getPostsWithSubstrInContent author page c
        ("name_substr", Just author) ->
          respondJson <$> P.getPostsWithSubstrInName author page c
        ("tag", Just tag) -> do
          print "I'm here!"
          print page
          respondJson <$> P.getPostsWithTag tag page c
        ("tags_in", Just tagsIn) ->
          respondJson <$> P.getPostsTagsIn tagsIn page c
        ("tags_all", Just tagsAll) ->
          respondJson <$> P.getPostsTagsAll tagsAll page c
        ("created_at", Just date) ->
          respondJson <$> P.getPostsDate date page c
        ("created_at__lt", Just dateLt) ->
          respondJson <$> P.getPostsDateLt dateLt page c
        ("created_at__gt", Just dateGt) ->
          respondJson <$> P.getPostsDateGt dateGt page c
{-processFilterGetRequest ["posts"] [("author_name", Just author)] c =
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
  respondJson <$> P.getPostsDateGt dateGt c-}

{-
  let mPage = lookup "page" $ queryString request
      page = mPage >>=
        maybe Nothing (decode . B.fromStrict:: BS.ByteString -> Maybe Integer)
-}

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

modelError :: Response
modelError = responseLBS status422 [("Content-Type", "application/json")]
  "Invalid model!"

idError :: Response
idError = responseLBS status422 [("Content-Type", "application/json")]
  "Invalid model id!"
--notImplementedFeature = responseLBS status200 [("Content-Type", "text/plain")] "This feature is not yet implemented"