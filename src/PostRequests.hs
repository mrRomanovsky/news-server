{-# LANGUAGE OverloadedStrings #-}

module PostRequests (processPostRequest) where

import Prelude hiding (read)
import qualified Tag as T
import qualified PostDTO as P
import qualified Post as PS
import qualified Draft as D
import qualified User as U
import qualified Author as A
import qualified Category as C
import Model
import Control.Monad (join)
import DbRequests
import Data.Text hiding (head)
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson
import Network.Wai
import Network.HTTP.Types (status200, status422, hAuthorization, Query)
import qualified Data.ByteString.Lazy as B
import Database.PostgreSQL.Simple hiding (Query)

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
    ("posts" : postId : "comments" : xs) -> postComment xs postId rBody c

postComment :: [Text] -> Text -> B.ByteString -> Connection -> IO Response
postComment [] pId c conn = P.insertComment pId (B.toStrict c) conn >> commentUpdated
postComment ["delete"] pId cId conn = P.deleteComment pId (B.toStrict cId) conn >> commentDeleted

commentUpdated :: IO Response
commentUpdated = return $ responseLBS status200 [("Content-Type", "application/json")]
  "Model was successfully updated"

commentDeleted :: IO Response
commentDeleted = return $ responseLBS status200 [("Content-Type", "application/json")]
  "Model was successfully deleted"

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

--return 40X in other cases
decodeUser = eitherDecode :: B.ByteString -> Either String U.User
decodeAuthor = eitherDecode :: B.ByteString -> Either String A.Author
decodeTag = eitherDecode :: B.ByteString -> Either String T.Tag
decodeCategory = eitherDecode :: B.ByteString -> Either String C.Category
decodeDraft = eitherDecode :: B.ByteString -> Either String D.Draft

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