{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Blog.Models.Author where

import Blog.Models.Model
import Blog.Models.User
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import Blog.ServerDB.DbRequests

data Author = Author
  { authorId :: Maybe AuthorId
  , userId :: Integer
  , desc :: Maybe Text
  } deriving (Show, Generic)

newtype AuthorId = AuthorId
  { aId :: Integer
  }

instance Show AuthorId where
  show = show . aId

instance ToJSON AuthorId where
  toJSON = toJSON . aId

instance FromJSON AuthorId where
  parseJSON = fmap AuthorId . parseJSON

instance FromField AuthorId where
  fromField field mdata =
    AuthorId <$> fromField field mdata

instance ToField AuthorId where
  toField = toField . aId

instance FromJSON Author where
  parseJSON (Object v) =
    Author <$> v .:? "authorId" <*> v .: "userId" <*>
    v .:? "desc"
  parseJSON _ = mzero

instance ToJSON Author

instance Model Author AuthorId where
  create Author {Blog.Models.Author.userId = uId, desc = aDesc} conn =
    void $
    execute
      conn
      "INSERT INTO authors(\"user_id\", author_desc) values (?,?)"
      (uId, aDesc)
  read = getRecords "authors"
  update Author {authorId = aId, Blog.Models.Author.userId = uId, desc = aDesc} conn =
    void $
    execute
      conn
      "UPDATE authors SET \"user_id\"=?, author_desc=? WHERE author_id=?"
      (uId, aDesc, aId)
  delete aId conn =
    void $ execute conn "DELETE FROM authors WHERE author_id=?" [aId]

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field
