{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Draft where
import qualified PostDTO as P
import qualified User as U
import Data.Aeson
import DbRequests
import Control.Applicative
import Control.Monad
import GHC.Generics
import Data.Text
import Data.Vector
import Model
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import qualified Database.PostgreSQL.Simple.Time as T

data Draft = Draft { draftId :: DraftId, postId, authorId :: Integer, postName :: Text, creationTime :: T.LocalTimestamp
                   , categoryId :: Integer, tags :: Maybe (Vector Integer)
                   , textContent :: Text, mainPhoto :: Text
                   , additionalPhotos :: Maybe (Vector Text), postComments :: Maybe (Vector Text)} deriving (Show, Generic)

newtype DraftId = DraftId {dId :: Integer}

instance Show DraftId where
  show = show . dId

instance ToJSON DraftId where
  toJSON = toJSON . dId

instance FromJSON DraftId where
  parseJSON  = fmap DraftId . parseJSON

instance FromField DraftId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ DraftId x

instance ToField DraftId where
  toField = toField . dId

instance Model Draft DraftId where
  create Draft{ Draft.postId = pId, Draft.authorId = aId, Draft.postName = dName
                 , Draft.creationTime = dTime, Draft.categoryId = cId
                 , Draft.tags = dTags, Draft.textContent = dText
                 , Draft.mainPhoto = dPhoto, Draft.additionalPhotos = dAddPhotos} conn = do
    execute conn "INSERT INTO drafts(post_id, author_id, draft_name, category_id, draft_tags, draft_text_content, draft_main_photo, draft_additional_photos) values (?, ?, ?, ?, ?, ?, ?, ?)"
      (pId, aId, dName, cId, dTags, dText, dPhoto, dAddPhotos)
    return () 

  read = getRecords "drafts"

  update Draft{ draftId = dId, Draft.postId = pId, Draft.authorId = aId, Draft.postName = dName
              , Draft.creationTime = dTime, Draft.categoryId = cId
              , Draft.tags = dTags, Draft.textContent = dText
              , Draft.mainPhoto = dPhoto, Draft.additionalPhotos = dAddPhotos} conn = do
    execute conn "UPDATE drafts SET post_id=?, author_id=?, draft_name=?, category_id=?, draft_tags=?, draft_text_content=?, draft_main_photo=?, draft_additional_photos=? WHERE draft_id=?"
       (pId, aId, dName, cId, dTags, dText, dPhoto, dAddPhotos, dId)
    return ()

  delete dId conn = do
    execute conn "DELETE FROM drafts WHERE draft_id=?" [dId]
    return ()

instance FromJSON Draft where
  parseJSON (Object v) = Draft <$> (v .: "draftId" <|> pure (DraftId (-1))) 
    <*> v .: "postId" <*> pure (-1) <*> v .: "postName"
    <*> (v .: "creationTime" <|> pure (U.getLocTimestamp "2017-07-28 14:14:14")) <*> v .: "categoryId"
    <*> v .:? "tags" <*> v .: "textContent" <*> v .: "mainPhoto" <*> v .:? "additionalPhotos"
    <*> v .:? "postComments" 
  parseJSON _ = mzero

instance ToJSON Draft

instance FromRow Draft where
  fromRow = Draft <$> field <*> field <*> field
    <*> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field

publishDraft :: DraftId -> Connection -> IO ()
publishDraft (DraftId did) conn = do
  execute conn
   "UPDATE posts SET (post_creation_time,\
                     \post_name,\
                     \category_id,\
                     \post_tags,\
                     \post_text_content,\
                     \post_main_photo,\
                     \post_additional_photos) =\
                 \(SELECT draft_creation_time, draft_name, category_id, draft_tags, draft_text_content, draft_main_photo, draft_additional_photos\
                 \   FROM drafts WHERE draft_id=?)\
                 \WHERE post_id = (SELECT post_id FROM drafts WHERE draft_id=?)" (did, did)
  return () --OPTIMIZE THIS FUNCTION! NOT EFFICIENT! TWO SAME SELECTS!