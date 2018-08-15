{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.Tag where

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
import ServerDB.DbRequests
import GHC.Generics
import Models.Model

data Tag = Tag
  { tagId :: TagId
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
  fromField field mdata = do
    x <- fromField field mdata
    return $ TagId x

instance ToField TagId where
  toField = toField . tId

instance ToJSON Tag

instance FromJSON Tag where
  parseJSON (Object v) =
    Tag <$> (v .: "tagId" <|> pure (TagId (-1))) <*> v .: "tagName"
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
