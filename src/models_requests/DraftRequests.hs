{-# LANGUAGE OverloadedStrings #-}
module DraftRequests where

import qualified Draft as D
import Control.Monad (join)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Text hiding (filter, head)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple hiding (Query)
import DbRequests
import Model
import Network.HTTP.Types (Query, hAuthorization, status200, status422)
import Network.Wai
import qualified Post as PS
import qualified PostDTO as P
import Prelude hiding (read)
import qualified Tag as T
import RequestsUtils

type DraftIdAction = Either String D.DraftId -> Connection -> IO Response

getDrafts :: Request -> Connection -> IO Response
getDrafts request =
  let (page, sortBy, auth) = getAdditionalParams request
      in fmap respondJson .
          getDraftsAuth auth (read page sortBy)

getDraftsAuth ::
     Maybe AuthData
  -> (Connection -> IO [D.Draft])
  -> Connection
  -> IO [D.Draft]
getDraftsAuth Nothing _ _ = return []
getDraftsAuth (Just auth) getDrafts conn = do
  authorId <- getAuthorId auth conn
  drafts <- getDrafts conn
  return $ maybe [] (`draftsForAuthor` drafts) authorId

draftsForAuthor :: Integer -> [D.Draft] -> [D.Draft]
draftsForAuthor aId = filter ((== aId) . D.authorId)

createDraft :: Request -> Connection -> IO Response
createDraft = draftModelAction createModel

draftWithAuthor :: B.ByteString -> Integer -> Either String D.Draft
draftWithAuthor d aId = setDraftAuthor aId <$> decodeDraft d

setDraftAuthor :: Integer -> D.Draft -> D.Draft
setDraftAuthor aId d = d {D.authorId = aId}

deleteDraft :: Request -> Connection -> IO Response
deleteDraft request c = do
  let auth = lookup hAuthorization $ requestHeaders request
  dId <- strictRequestBody request
  aId <- maybe (return Nothing) (`getAuthorId` c) auth
  maybe (return notFound) (\a -> draftIdAction dId a deleteModel c) aId

updateDraft :: Request -> Connection -> IO Response
updateDraft = draftModelAction updateModel

draftModelAction :: (Either String D.Draft -> Connection -> IO Response) -> Request -> Connection -> IO Response
draftModelAction act request c = do
  let auth = lookup hAuthorization $ requestHeaders request
  d <- strictRequestBody request
  aId <- maybe (return Nothing) (`getAuthorId` c) auth
  maybe (return notFound) (\a -> act (draftWithAuthor d a) c) aId

publishDraft :: Request -> Connection -> IO Response
publishDraft request c = do
  let auth = lookup hAuthorization $ requestHeaders request
  dId <- strictRequestBody request
  aId <- maybe (return Nothing) (`getAuthorId` c) auth
  maybe (return notFound) (\a -> draftIdAction dId a publishDraftRequest c) aId

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