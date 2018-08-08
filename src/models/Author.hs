{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Author where

import User
import Model
import Data.Aeson
import Control.Applicative
import Control.Monad
import GHC.Generics
import DbRequests
import Data.Text
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

data Author = Author {authorId :: AuthorId, userId :: Integer, desc :: Maybe Text} deriving (Show, Generic)

newtype AuthorId = AuthorId {aId :: Integer}

instance Show AuthorId where
  show = show . aId

instance ToJSON AuthorId where
  toJSON = toJSON . aId

instance FromJSON AuthorId where
  parseJSON = fmap AuthorId . parseJSON

instance FromField AuthorId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ AuthorId x

instance ToField AuthorId where
  toField = toField . aId

instance FromJSON Author where
  parseJSON (Object v) = Author <$> (v .: "authorId" <|> pure (AuthorId (-1))) <*> v .: "userId" <*> v .:? "desc"
  parseJSON _ = mzero
instance ToJSON Author

instance Model Author AuthorId where
  create Author{Author.userId = uId, Author.desc = aDesc} conn = do
    execute conn "INSERT INTO authors(\"user_id\", author_desc) values (?,?)"
      (uId, aDesc)
    return ()

  read = getRecords "authors"  

  update Author{Author.authorId = aId, Author.userId = uId, Author.desc = aDesc} conn = do
    execute conn "UPDATE authors SET \"user_id\"=?, author_desc=? WHERE author_id=?"
      (uId, aDesc, aId)
    return ()
    
  delete aId conn = do
    execute conn "DELETE FROM authors WHERE author_id=?" [aId]
    return ()

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field