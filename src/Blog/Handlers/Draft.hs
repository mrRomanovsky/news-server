{-# LANGUAGE OverloadedStrings #-}

module Blog.Handlers.Draft
  ( getDrafts
  , createDraft
  , updateDraft
  , deleteDraft
  , publishDraft
  ) where

import Blog.Exceptions.Exceptions
import Blog.Handlers.HandlersUtils
import qualified Blog.Models.Draft as D
import Blog.ServerDB.Author
import Blog.ServerDB.DbRequests
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Database.PostgreSQL.Simple
import Network.HTTP.Types (hAuthorization, status200)
import Network.Wai
import Prelude hiding (read)

type DraftIdAction = Either String D.DraftId -> Connection -> IO Response

getDrafts :: Request -> Connection -> IO Response
getDrafts request =
  let (page, sortBy, auth) = getAdditionalParams request
   in case auth of
        (Just aId) ->
          fmap respondJson .
          (getRecordsWhere "drafts" page sortBy (Just ("author_id", aId)) :: Connection -> IO [D.Draft])
        _ -> const $ return notFound

createDraft :: Request -> Connection -> IO Response
createDraft = draftModelAction createModel

draftWithAuthor :: B.ByteString -> Integer -> Either String D.Draft
draftWithAuthor d aId = setDraftAuthor aId <$> decodeDraft d

setDraftAuthor :: Integer -> D.Draft -> D.Draft
setDraftAuthor aId d = d {D.authorId = aId}

deleteDraft :: Request -> Connection -> IO Response
deleteDraft request c = do
  (auth, dId, aId) <- getDraftData request c
  maybe (return notFound) (\a -> draftIdAction dId a deleteModel c) aId

updateDraft :: Request -> Connection -> IO Response
updateDraft = draftModelAction updateModel

draftModelAction ::
     (Either String D.Draft -> Connection -> IO Response)
  -> Request
  -> Connection
  -> IO Response
draftModelAction act request c = do
  let auth = lookup hAuthorization $ requestHeaders request
  d <- strictRequestBody request
  aId <- maybe (return Nothing) (`getAuthorId` c) auth
  maybe (return notFound) (\a -> act (draftWithAuthor d a) c) aId

publishDraft :: Request -> Connection -> IO Response
publishDraft request c = do
  (auth, dId, aId) <- getDraftData request c
  maybe (return notFound) (\a -> draftIdAction dId a publishDraftRequest c) aId

getDraftData request c = do
  let auth = lookup hAuthorization $ requestHeaders request
  dId <- strictRequestBody request
  aId <- maybe (return Nothing) (`getAuthorId` c) auth
  return (auth, dId, aId)

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
