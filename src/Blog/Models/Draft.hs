{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Blog.Models.Draft where

import Blog.Models.Model
import qualified Blog.Models.PostDTO as P
import qualified Blog.Models.User as U
import Blog.ServerDB.DbRequests
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Maybe (isNothing)
import Data.Text
import Data.Vector hiding ((++))
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import qualified Database.PostgreSQL.Simple.Time as T
import Database.PostgreSQL.Simple.ToField
import GHC.Generics

data Draft = Draft
  { draftId :: Maybe DraftId
  , postId :: Maybe Integer
  , authorId :: Integer
  , postName :: Text
  , creationTime :: Maybe T.LocalTimestamp
  , categoryId :: Integer
  , tags :: Maybe (Vector Integer)
  , textContent :: Text
  , mainPhoto :: Text
  , additionalPhotos :: Maybe (Vector Text)
  , postComments :: Maybe (Vector Text)
  } deriving (Show, Generic)

newtype DraftId = DraftId
  { dId :: Integer
  }

instance Show DraftId where
  show = show . dId

instance ToJSON DraftId where
  toJSON = toJSON . dId

instance FromJSON DraftId where
  parseJSON = fmap DraftId . parseJSON

instance FromField DraftId where
  fromField field mdata = DraftId <$> fromField field mdata

instance ToField DraftId where
  toField = toField . dId

instance Model Draft DraftId where
  create Draft { postId = pId
               , authorId = aId
               , postName = dName
               , creationTime = dTime
               , categoryId = cId
               , tags = dTags
               , textContent = dText
               , mainPhoto = dPhoto
               , additionalPhotos = dAddPhotos
               } conn =
    if isNothing pId --inserting draft without existing post 
      then void $
           execute
             conn
             "INSERT INTO drafts(author_id, draft_name, category_id, draft_tags, draft_text_content, draft_main_photo, draft_additional_photos) values (?, ?, ?, ?, ?, ?, ?, ?)"
             (aId, dName, cId, dTags, dText, dPhoto, dAddPhotos)
      else void $
           execute
             conn
             "INSERT INTO drafts(post_id, author_id, draft_name, category_id, draft_tags, draft_text_content, draft_main_photo, draft_additional_photos) values (?, ?, ?, ?, ?, ?, ?, ?)"
             (pId, aId, dName, cId, dTags, dText, dPhoto, dAddPhotos)
  getData = getRecords "drafts"
  update Draft { draftId = dId
               , postId = pId
               , authorId = aId
               , postName = dName
               , creationTime = dTime
               , categoryId = cId
               , tags = dTags
               , textContent = dText
               , mainPhoto = dPhoto
               , additionalPhotos = dAddPhotos
               } conn =
    if isNothing pId
      then void $
           execute
             conn
             "UPDATE drafts SET author_id=?, draft_name=?, category_id=?, draft_tags=?, draft_text_content=?, draft_main_photo=?, draft_additional_photos=? WHERE draft_id=?"
             (aId, dName, cId, dTags, dText, dPhoto, dAddPhotos, dId)
      else void $
           execute
             conn
             "UPDATE drafts SET post_id=?, author_id=?, draft_name=?, category_id=?, draft_tags=?, draft_text_content=?, draft_main_photo=?, draft_additional_photos=? WHERE draft_id=?"
             (pId, aId, dName, cId, dTags, dText, dPhoto, dAddPhotos, dId)
  delete dId conn = do
    execute conn "DELETE FROM drafts WHERE draft_id=?" [dId]
    return ()

instance FromJSON Draft where
  parseJSON (Object v) =
    Draft <$> v .:? "draftId" <*> v .:? "postId" <*> pure (-1) <*>
    v .: "postName" <*>
    v .:? "creationTime" <*>
    v .: "categoryId" <*>
    v .:? "tags" <*>
    v .: "textContent" <*>
    v .: "mainPhoto" <*>
    v .:? "additionalPhotos" <*>
    v .:? "postComments"
  parseJSON _ = mzero

instance ToJSON Draft

instance FromRow Draft where
  fromRow =
    Draft <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*>
    field <*>
    field <*>
    field <*>
    field

getUserDrafts :: Integer -> Connection -> IO [Draft]
getUserDrafts uId conn =
  query
    conn
    "SELECT * FROM drafts \
    \WHERE author_id = (SELECT author_id FROM authors \
      \WHERE user_id = ?"
    [uId]

publishDraft :: DraftId -> Connection -> IO ()
publishDraft (DraftId dId) conn =
  catch (void $ execute conn "SELECT publish_draft(?)" [dId]) handleFuncCall

handleFuncCall :: QueryError -> IO () --I just couldn't normally call function with "execute"
handleFuncCall e = print $ "error occured during call to function (but everything is ok): " ++ show e
