{-# LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tag where

import DbRequests
import Model
import Data.Text
import GHC.Generics
import Data.Aeson
import Control.Monad
import Control.Applicative
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField

data Tag = Tag {tagId :: TagId, tagName :: Text} deriving (Show, Generic)

newtype TagId = TagId {tId :: Integer}

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
  parseJSON (Object v) = Tag <$> (v .: "tagId" <|> pure (TagId (-1))) <*> v .: "tagName"
  parseJSON _ = mzero


{-instance FromField Text where
  fromField f Nothing = returnError ConversionFailed f "Nothing returned for A"
  fromField f (Just bs) = return $ decodeUtf8 bs-}


instance FromRow Tag where
  fromRow = Tag <$> field <*> field

instance Model Tag TagId where
  create Tag{Tag.tagName = n} conn = do
    execute conn "INSERT INTO tags(tag_name) values (?)"
             [n]
    return ()

  read = getRecords "tags"

  update Tag{Tag.tagId = tId, Tag.tagName = tName} conn = do
    execute conn "UPDATE tags SET tag_name=? WHERE tag_id=?" (tName, tId)
    return () --maybe I should do something with execute to remove this "return"

  delete tId conn = do
    execute conn "DELETE FROM tags WHERE tag_id=?" [tId]
    return ()

{-select * from foo where 'abc' = ANY(stuff);-}