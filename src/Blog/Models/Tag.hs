{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Blog.Models.Tag where

import Blog.Models.Model
import Blog.ServerDB.DbRequests
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import GHC.Generics

data Tag = Tag
  { tagId :: Maybe TagId
  , tagName :: Text
  } deriving (Show, Generic)

newtype TagId = TagId
  { tId :: Integer
  }

instance Show TagId where
  show = show . tId

instance ToJSON TagId where
  toJSON = toJSON . tId

instance FromJSON TagId where
  parseJSON = fmap TagId . parseJSON

instance FromField TagId where
  fromField field mdata = TagId <$> fromField field mdata

instance ToField TagId where
  toField = toField . tId

instance ToJSON Tag

instance FromJSON Tag where
  parseJSON (Object v) = Tag <$> v .:? "tagId" <*> v .: "tagName"
  parseJSON _ = mzero

instance FromRow Tag where
  fromRow = Tag <$> field <*> field

instance Model Tag TagId where
  create Tag {tagName = n} conn =
    void $ execute conn "INSERT INTO tags(tag_name) values (?)" [n]
  read = getRecords "tags"
  update Tag {tagId = tId, tagName = tName} conn =
    void $ execute conn "UPDATE tags SET tag_name=? WHERE tag_id=?" (tName, tId)
  delete tId conn = void $ execute conn "DELETE FROM tags WHERE tag_id=?" [tId]
