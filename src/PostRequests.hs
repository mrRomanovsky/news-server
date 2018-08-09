{-# LANGUAGE OverloadedStrings #-}

module PostRequests
  ( processPostRequest
  ) where

import qualified Author as A
import qualified Category as C
import Control.Monad (join)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text hiding (head)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple hiding (Query)
import DbRequests
import qualified Draft as D
import Model
import Network.HTTP.Types (Query, hAuthorization, status200, status422)
import Network.Wai
import qualified Post as PS
import qualified PostDTO as P
import Prelude hiding (read)
import qualified Tag as T
import qualified User as U

processPostRequest :: Request -> Connection -> IO Response
processPostRequest request c = do
  rBody <- strictRequestBody request
  let auth = lookup hAuthorization $ requestHeaders request
  case pathInfo request of
    ("users":xs) -> postUser xs rBody auth c
    ("tags":xs) -> authResponse auth (postTag xs rBody) c
    ("authors":xs) -> authResponse auth (postAuthor xs rBody) c
    ("categories":xs) -> authResponse auth (postCategory xs rBody) c
    ("drafts":xs) -> postDraft xs rBody auth c
    ("posts":postId:"comments":xs) -> postComment xs postId rBody c
    _ -> return notFound

postComment :: [Text] -> Text -> B.ByteString -> Connection -> IO Response
postComment [] pId c conn =
  P.insertComment pId (B.toStrict c) conn >> commentUpdated
postComment ["delete"] pId cId conn =
  P.deleteComment pId (B.toStrict cId) conn >> commentDeleted
postComment _ _ _ _ = return notFound

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

postTag :: [Text] -> B.ByteString -> Connection -> IO Response
postTag [] t = createModel $ decodeTag t
postTag ["update"] t = updateModel $ decodeTag t
postTag ["delete"] tId = deleteModel (eitherDecode tId :: Either String T.TagId)
postTag _ _ = const $ return notFound

postCategory :: [Text] -> B.ByteString -> Connection -> IO Response
postCategory [] c = createModel $ decodeCategory c
postCategory ["update"] c = updateModel $ decodeCategory c
postCategory ["delete"] cId =
  deleteModel (eitherDecode cId :: Either String C.CategoryId)
postCategory _ _ = const $ return notFound

postUser ::
     [Text] -> B.ByteString -> Maybe AuthData -> Connection -> IO Response
postUser [] u _ = createModel $ decodeUser u
postUser ["delete"] uId auth =
  authResponse auth $ deleteModel (eitherDecode uId :: Either String U.UserId)
postUser _ _ _ = const $ return notFound

postDraft ::
     [Text] -> B.ByteString -> Maybe AuthData -> Connection -> IO Response
postDraft path d auth c = do
  aId <- maybe (return Nothing) (`getAuthorId` c) auth
  maybe (return notFound) (\a -> postDraftUser path d a c) aId

postDraftUser :: [Text] -> B.ByteString -> Integer -> Connection -> IO Response
postDraftUser [] d aId = createModel $ draftWithAuthor d aId
postDraftUser ["update"] d aId = updateModel $ draftWithAuthor d aId
postDraftUser ["delete"] dId aId = draftIdAction dId aId deleteModel
postDraftUser ["publish"] dId aId = draftIdAction dId aId publishDraftRequest
postDraftUser _ _ _ = const $ return notFound

draftWithAuthor :: B.ByteString -> Integer -> Either String D.Draft
draftWithAuthor d aId = setDraftAuthor aId <$> decodeDraft d

draftIdAction ::
     B.ByteString -> Integer -> DraftIdAction -> Connection -> IO Response
draftIdAction dId aId action c = do
  let draftId = eitherDecode dId :: Either String D.DraftId
  either
    (const $ return notFound)
    (\(D.DraftId drId) -> do
       draftAuthor <- getDraftAuthor drId c
       if draftAuthor == aId
         then action draftId c
         else return notFound)
    draftId

setDraftAuthor :: Integer -> D.Draft -> D.Draft
setDraftAuthor aId d = d {D.authorId = aId}

postAuthor :: [Text] -> B.ByteString -> Connection -> IO Response
postAuthor [] a = createModel $ decodeAuthor a
postAuthor ["update"] a = updateModel $ decodeAuthor a
postAuthor ["delete"] aId =
  deleteModel (eitherDecode aId :: Either String A.AuthorId)
postAuthor _ _ = const $ return notFound

type DraftIdAction = Either String D.DraftId -> Connection -> IO Response

publishDraftRequest :: Either String D.DraftId -> Connection -> IO Response
publishDraftRequest dId c = do
  either
    (\e -> print $ "error parsing draft id: " ++ e)
    (`D.publishDraft` c)
    dId
  return $
    responseLBS
      status200
      [("Content-Type", "application/json")]
      "Draft was successfully published"

decodeUser = eitherDecode :: B.ByteString -> Either String U.User

decodeAuthor = eitherDecode :: B.ByteString -> Either String A.Author

decodeTag = eitherDecode :: B.ByteString -> Either String T.Tag

decodeCategory = eitherDecode :: B.ByteString -> Either String C.Category

decodeDraft = eitherDecode :: B.ByteString -> Either String D.Draft

updateModel :: Model m id => Either String m -> Connection -> IO Response
updateModel model conn =
  either processBadModel ((>> return responseUpdated) . (`update` conn)) model

responseUpdated =
  responseLBS
    status200
    [("Content-Type", "application/json")]
    "Model was successfully updated"

deleteModel :: Model m id => Either String id -> Connection -> IO Response
deleteModel mId conn =
  either processBadModel ((>> return responseDeleted) . (`delete` conn)) mId

responseDeleted =
  responseLBS
    status200
    [("Content-Type", "application/json")]
    "Model was successfully deleted from the database"

createModel :: Model m id => Either String m -> Connection -> IO Response
createModel model conn =
  either processBadModel ((>> return responseAdded) . (`create` conn)) model

responseAdded =
  responseLBS
    status200
    [("Content-Type", "application/json")]
    "Model was successfully added to the database"

modelError :: Response
modelError =
  responseLBS status422 [("Content-Type", "application/json")] "Invalid model!"

processBadModel :: String -> IO Response
processBadModel e = do
  appendFile "news-server.log" $ "\nerror processing request body: " ++ e
  return errorResponse

errorResponse :: Response
errorResponse =
  responseLBS status422 [("Content-Type", "text/plain")] "Bad model/id data!"
